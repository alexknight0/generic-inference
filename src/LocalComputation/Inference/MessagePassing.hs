{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LocalComputation.Inference.MessagePassing (
      messagePassing
    , NodeWithProcessId (id, node)
    , NodeActions
    , SerializableValuation
) where

import           Control.Distributed.Process              hiding (Message)
import           Control.Distributed.Process.Serializable

import qualified Algebra.Graph                            as DG
import qualified Algebra.Graph.Undirected                 as UG
import           Control.DeepSeq                          (NFData)
import           Control.Monad                            (forM_)
import           Data.Binary                              (Binary)
import qualified Data.Set                                 as S
import           GHC.Generics                             (Generic)
import qualified LocalComputation.Inference.JoinTree      as JT
import qualified LocalComputation.Utils                   as U
import qualified LocalComputation.ValuationAlgebra        as V
import           Type.Reflection                          (Typeable)


type SerializableValuation v a = (V.Valuation v, V.Var a, Binary (v a), Typeable v, Typeable a)

data NodeWithProcessId a = NodeWithProcessId { id :: ProcessId, node :: JT.Node a } deriving (Generic, Binary)

type NodeActions v a = NodeWithProcessId (v a) -> [NodeWithProcessId (v a)] -> SendPort (JT.Node (v a)) -> Process ()

-- TODO: This function shouldn't burden itself with the responsibility of taking a directed graph as input
-- and returning a directed graph as output. The message passing algorithm at this stage treats its graph
-- as undirected and this should be reflected in the function signature. Other functions can wrap this
-- function to perform graph reconstruction if required.
{- | Executes a message passing algorithm on a given join tree by spinning up each node in a join tree as
a seperate process and allowing each process to execute the given `nodeActions`.

Each node is respresented by a `NodeWithProcessId`. Each node is given the `NodeWithProcessId` of itself as
well as its neighbours and can communicate with them by `send` and `expect`. All nodes are expected to send
a result through a given `SendPort` that represents their state after the message passing is completed.
-}
-- This function can be extended to work on directed graphs if required by seperating the neighbours
-- sent to each node into 'incoming' and 'outgoing' neighbours
messagePassing :: forall v a. (SerializableValuation v a)
    => DG.Graph (JT.Node (v a))
    -> NodeActions v a
    -> Process (DG.Graph (JT.Node (v a)))
messagePassing directed nodeActions = do

    -- Initialize all nodes
    let undirected = UG.toUndirected directed
        vs         = UG.vertexList undirected
    (nodesWithPid, resultPorts) <- fmap unzip $ mapM (initializeNodeAndMonitor nodeActions) vs

    -- Tell each node who it is and who it's neighbours are
    forM_ nodesWithPid $ \nodeWithPid -> do
        let neighbours        = S.toList $ UG.neighbours nodeWithPid.node undirected
            neighboursWithPid = filter (\n -> n.node `elem` neighbours) nodesWithPid
        send nodeWithPid.id nodeWithPid
        send nodeWithPid.id neighboursWithPid

    -- Wait for normal termination
    forM_ nodesWithPid $ \_ -> do
        (ProcessMonitorNotification _ _ reasonForTermination) <- expect
        case reasonForTermination of
             DiedNormal      -> pure ()
             DiedException e -> error $ "Error - DiedException (" ++ e ++ ")"
             -- TODO: Does 'DiedUnknownId' indicate that the process died *before*
             -- we got a chance to wait on it?
             x               -> error $ "Error - " ++ show x

    -- All should be terminated - receive all messages
    newNodes <- mapM receiveChanNowA resultPorts

    -- Construct graph from new nodes
    pure $ fmap (\oldNode -> U.unsafeFind (\newNode -> newNode.id == oldNode.id) newNodes) directed

initializeNodeAndMonitor :: (SerializableValuation v a)
    => NodeActions v a
    -> JT.Node (v a)
    -> Process (NodeWithProcessId (v a), ReceivePort (JT.Node (v a)))
initializeNodeAndMonitor nodeActions node = do
    (sendFinalResult, receiveFinalResult) <- newChan

    i <- initializeNode nodeActions sendFinalResult
    _ <- monitor i

    pure (NodeWithProcessId i node, receiveFinalResult)

initializeNode :: (SerializableValuation v a)
    => NodeActions v a
    -> SendPort (JT.Node (v a))
    -> Process ProcessId
initializeNode nodeActions resultPort = spawnLocal $ do
    this :: NodeWithProcessId (v a) <- expect
    neighbours :: [NodeWithProcessId (v a)] <- expect

    nodeActions this neighbours resultPort

receiveChanNowA :: (Serializable a)
    => ReceivePort a -> Process a
receiveChanNowA p = do
    result <- receiveChanTimeout 0 p
    pure $ case result of
                Just x -> x
                Nothing -> error "A node terminated without sending a message on the result channel"

