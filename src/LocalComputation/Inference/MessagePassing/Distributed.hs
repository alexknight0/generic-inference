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
import qualified LocalComputation.Inference.JoinTree        as JF
import qualified LocalComputation.Inference.JoinTree.Forest as JF
import qualified LocalComputation.Inference.JoinTree.Tree   as JT
import qualified LocalComputation.Utils                     as U
import           LocalComputation.ValuationAlgebra          (Binary, Generic,
                                                             Typeable)
import qualified LocalComputation.ValuationAlgebra          as V

type SerializableValuation v a = (V.Valuation v a, Binary (v a), Typeable v, Typeable a, Binary a)

data NodeWithPid v a = NodeWithPid { id :: ProcessId, node :: JF.Node v a } deriving (Generic, Binary)

type NodeActions v a = NodeWithPid v a -> [NodeWithPid v a] -> SendPort (JF.Node v a) -> Process ()

{- | Executes a message passing algorithm on a given join tree by spinning up each node in a join tree as
a seperate process and allowing each process to execute the given `nodeActions`.

Each node is respresented by a `NodeWithPid`. Each node is given the `NodeWithPid` of itself as
well as its neighbours and can communicate with them by `send` and `expect`. All nodes are expected to send
a result through a given `SendPort` that represents their state after the message passing is completed.

Only trees of the forest that have query nodes have message passing performed on them.
Only query nodes will have their valuations updated, although this can be easily changed.
-}
messagePassing :: forall v a. (SerializableValuation v a)
    => JF.JoinForest v a
    -> NodeActions v a
    -> Process (JF.JoinForest v a)
messagePassing forest nodeActions = do

    -- Perform message passing on all trees that have query nodes.
    requiredUpdates <- mapM (\t -> messagePassing'' t nodeActions) $ filter JT.hasQueryNode $ JF.treeList forest

    -- Update forest with required changes.
    pure $ JF.unsafeUpdateValuations (M.unions requiredUpdates) forest

-- | Variant of `messagePassing` that operates on a `JoinTree` parameter.
messagePassing' :: (SerializableValuation v a)
    => JF.JoinTree v a
    -> NodeActions v a
    -> Process (JF.JoinTree v a)
messagePassing' tree nodeActions = do
    -- Perform message passing on tree.
    requiredUpdates <- messagePassing'' tree nodeActions

    -- Update tree with required changes.
    pure $ JT.unsafeUpdateValuations requiredUpdates tree


-- | Variant of `messagePassing` that takes a `JoinTree` parameter and returns the
-- required updates to the join tree after message passing.
messagePassing'' :: (SerializableValuation v a)
    => JF.JoinTree v a
    -> NodeActions v a
    -> Process (M.Map JT.Id (v a))
messagePassing'' tree nodeActions = do

    -- Initialize all nodes
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

    -- All should be terminated.
    -- We should receive a message from each query node.
    -- We may or may not also receive messages from other nodes wishing to update their valuation.
    newNodeList <- fmap U.onlyJust $ mapM receiveChanNow resultPorts
    assertCorrectNumResponses newNodeList

    -- Return updates required for the new join tree.
    pure $ toMap newNodeList

    where
        vs = JT.vertexList tree

        assertCorrectNumResponses responses = assert (length responses >= length (filter JT.isQueryNode vs)) $ pure ()

        neighbourMap = JT.neighbourMap tree

        toMap = M.fromList . map (\n -> (n.id, n.v))

initializeNodeAndMonitor :: (SerializableValuation v a)
    => NodeActions v a
    -> JF.Node v a
    -> Process (NodeWithPid v a, ReceivePort (JF.Node v a))
initializeNodeAndMonitor nodeActions node = do
    (sendFinalResult, receiveFinalResult) <- newChan

    i <- initializeNode nodeActions sendFinalResult
    _ <- monitor i

    pure (NodeWithPid i node, receiveFinalResult)

initializeNode :: (SerializableValuation v a)
    => NodeActions v a
    -> SendPort (JF.Node v a)
    -> Process ProcessId
initializeNode nodeActions resultPort = spawnLocal $ do
    this :: NodeWithPid v a <- expect
    neighbours :: [NodeWithPid v a] <- expect

    nodeActions this neighbours resultPort

receiveChanNow :: (Serializable a)
    => ReceivePort a -> Process (Maybe a)
receiveChanNow p = do
    result <- receiveChanTimeout 0 p
    pure $ result

data Message a = Message {
          sender :: ProcessId
        , msg    :: a
} deriving (Generic, Binary)

type ComputeMessage v a = [Message (v a)] -> NodeWithPid v a -> NodeWithPid v a -> v a

data CollectResults v a = CollectResults {
      target  :: NodeWithPid v a
    , postbox :: [Message (v a)]
}

-- TODO: Can place postbox on node.

-- | Performs the collect phase and calculate phase for a given node.
--
-- __Warning__: Assumes the target node is not the root node, and that the target node
-- has at least one neighbour.
collectAndCalculate :: forall v a . (SerializableValuation v a)
    => NodeWithPid v a
    -> [NodeWithPid v a]
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
    => NodeWithPid v a
    -> [NodeWithPid v a]
    -> ComputeMessage v a
    -> Process (CollectResults v a)
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

-- | The distribute phase of a message passing process where each node waits to receive a final message
-- and then distributes messages to all nodes it has not previously sent messages to.
--
-- __Warning__: Assumes the target node has at least one neighbour.
distribute :: (SerializableValuation v a)
    => CollectResults v a
    -> NodeWithPid v a
    -> [NodeWithPid v a]
    -> ComputeMessage v a
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

sendMsg :: (SerializableValuation v a) => NodeWithPid v a -> NodeWithPid v a -> v a -> Process ()
sendMsg sender target = send target.id . Message sender.id
