{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LocalComputation.Inference.MessagePassing.Distributed (
      messagePassing
    , messagePassing'
    , NodeWithPid (id, node)
    , NodeActions
    , SerializableValuation
    , Message (msg, sender, Message)
    , ComputeMessage
    , collect
    , CollectResults (target, postbox)
    , distribute
    , DistributeResults (postbox)
    , collectAndCalculate
) where

import           Control.Distributed.Process                hiding (Message)
import           Control.Distributed.Process.Serializable

import           Control.Exception                          (assert)
import           Control.Monad                              (forM_, replicateM)
import qualified Data.Map                                   as M
import qualified Data.Set                                   as S
import qualified LocalComputation.Inference.JoinTree        as JT
import qualified LocalComputation.Inference.JoinTree.Forest as JT
import qualified LocalComputation.Utils                     as U
import           LocalComputation.ValuationAlgebra          (Binary, Generic,
                                                             Typeable)
import qualified LocalComputation.ValuationAlgebra          as V

type SerializableValuation v a = (V.ValuationFamily v, V.Var a, Binary (v a), Typeable v, Typeable a)

data NodeWithPid a = NodeWithPid { id :: ProcessId, node :: JT.Node a } deriving (Generic, Binary)

type NodeActions v a = NodeWithPid (v a) -> [NodeWithPid (v a)] -> SendPort (JT.Node (v a)) -> Process ()

{- | Executes a message passing algorithm on a given join tree by spinning up each node in a join tree as
a seperate process and allowing each process to execute the given `nodeActions`.

Each node is respresented by a `NodeWithPid`. Each node is given the `NodeWithPid` of itself as
well as its neighbours and can communicate with them by `send` and `expect`. All nodes are expected to send
a result through a given `SendPort` that represents their state after the message passing is completed.
-}
-- This function can be extended to work on directed graphs if required by seperating the neighbours
-- sent to each node into 'incoming' and 'outgoing' neighbours
messagePassing :: forall v a. (SerializableValuation v a)
    => JT.JoinForest (v a)
    -> NodeActions v a
    -> Process (JT.JoinForest (v a))
messagePassing tree nodeActions = do

    -- Initialize all nodes
    let vs = JT.vertexList tree
    (nodesWithPid, resultPorts) <- fmap unzip $ mapM (initializeNodeAndMonitor nodeActions) vs

    -- Tell each node who it is and who it's neighbours are
    forM_ nodesWithPid $ \nodeWithPid -> do
        let neighbours        = neighbourMap M.! nodeWithPid.node.id
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
    newNodeList <- mapM receiveChanNowA resultPorts

    -- Construct graph from new nodes
    pure $ JT.unsafeUpdateValuations (toMap newNodeList) tree

    where
        neighbourMap = JT.neighbourMap tree

        toMap = M.fromList . map (\n -> (n.id, n.v))

-- | Variant of `messagePassing` that operates on a `JoinTree` parameter.
messagePassing' :: (SerializableValuation v a)
    => JT.JoinTree (v a)
    -> NodeActions v a
    -> Process (JT.JoinTree (v a))
messagePassing' tree nodeActions = fmap JT.unsafeGetTree $ messagePassing (JT.toForest tree) nodeActions

initializeNodeAndMonitor :: (SerializableValuation v a)
    => NodeActions v a
    -> JT.Node (v a)
    -> Process (NodeWithPid (v a), ReceivePort (JT.Node (v a)))
initializeNodeAndMonitor nodeActions node = do
    (sendFinalResult, receiveFinalResult) <- newChan

    i <- initializeNode nodeActions sendFinalResult
    _ <- monitor i

    pure (NodeWithPid i node, receiveFinalResult)

initializeNode :: (SerializableValuation v a)
    => NodeActions v a
    -> SendPort (JT.Node (v a))
    -> Process ProcessId
initializeNode nodeActions resultPort = spawnLocal $ do
    this :: NodeWithPid (v a) <- expect
    neighbours :: [NodeWithPid (v a)] <- expect

    nodeActions this neighbours resultPort

receiveChanNowA :: (Serializable a)
    => ReceivePort a -> Process a
receiveChanNowA p = do
    result <- receiveChanTimeout 0 p
    pure $ case result of
                Just x -> x
                Nothing -> error "A node terminated without sending a message on the result channel"

data Message a = Message {
          sender :: ProcessId
        , msg    :: a
} deriving (Generic, Binary)

type ComputeMessage a = [Message a] -> NodeWithPid a -> NodeWithPid a -> a

data CollectResults a = CollectResults {
      target  :: NodeWithPid a
    , postbox :: [Message a]
}

-- TODO: Can place postbox on node.

collectAndCalculate :: forall v a . (SerializableValuation v a)
    => NodeWithPid (v a)
    -> [NodeWithPid (v a)]
    -> Process (v a)
collectAndCalculate _    []         = error "Collect and calculate assumes node has at least one neighbour."
collectAndCalculate this neighbours = do
    -- Wait for messages from all neighbours bar one
    postbox <- replicateM (length neighbours - 1) expect :: Process [Message (v a)]

    let senders = map (.sender) postbox

        -- The target neighbour is the neighbour who didn't send a message to us
        target = U.findAssertSingleMatch (\n -> n.id `notElem` senders) neighbours

        newValuation = V.combines1 (this.node.v : map (.msg) postbox)
        messageForTarget = V.project newValuation (S.intersection this.node.d target.node.d)


    -- Perform some action with these messages and send to remaining neighbour
    sendMsg this target messageForTarget

    -- Return results
    pure $ newValuation

-- | The collect phase of a message passing process where each node waits for every neighbour bar one
-- to send the node a message before the node sends off a message to the neighbour that didn't send it
-- a message.
--
-- __Warning__: Assumes the target node has at least one neighbour.
collect :: (SerializableValuation v a)
    => NodeWithPid (v a)
    -> [NodeWithPid (v a)]
    -> ComputeMessage (v a)
    -> Process (CollectResults (v a))
collect _    []         _      = error "Collect assumes node has at least one neighbour."
collect this neighbours action = do
    -- Wait for messages from all neighbours bar one
    postbox <- replicateM (length neighbours - 1) expect

    let senders = map (.sender) postbox

        -- The target neighbour is the neighbour who didn't send a message to us
        target = U.findAssertSingleMatch (\n -> n.id `notElem` senders) neighbours

    -- Perform some action with these messages and send to remaining neighbour
    sendMsg this target (action postbox this target)

    -- Return results
    pure $ CollectResults target postbox

data DistributeResults a = DistributeResults {
    postbox :: [Message a]
}

-- TODO: There is likely a smarter way to do combines here to reduce duplication, but it seems
-- very difficult

-- | The distribute phase of a message passing process where each node waits to receive a final message
-- and then distributes messages to all nodes it has not previously sent messages to.
--
-- __Warning__: Assumes the target node has at least one neighbour.
distribute :: (SerializableValuation v a)
    => CollectResults (v a)
    -> NodeWithPid (v a)
    -> [NodeWithPid (v a)]
    -> ComputeMessage (v a)
    -> Process (DistributeResults (v a))
distribute _              _    []         _      = error "Distribute assumes node has at least one neighbour."
distribute collectResults this neighbours action = do
    -- Wait for response from neighbour we just sent a message to
    message :: Message (v a) <- expect
    assert (message.sender == collectResults.target.id) (pure ())

    let postbox = message : collectResults.postbox
        -- The target neighbours are the neighbours who were not the target of the collect phase
        targets = filter (\n -> n.id /= collectResults.target.id) neighbours

    -- Send out messages to remaining neighbours (which we now have enough information to send messages to)
    mapM_ (\target -> sendMsg this target $ action postbox this target)
          targets

    pure $ DistributeResults postbox

sendMsg :: Serializable a => NodeWithPid a -> NodeWithPid a -> a -> Process ()
sendMsg sender target = send target.id . Message sender.id
