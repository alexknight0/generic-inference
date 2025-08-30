{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LocalComputation.Inference.ShenoyShafer (
      initializeNodes
    , shenoyJoinTree
    , answerQueriesM, answerQueryM, answerQueriesDrawGraphM
    , answerQueries, answerQuery
    , inference
    , InferredData
) where

import           Control.Distributed.Process                 hiding (Message)
import           Control.Distributed.Process.Serializable

import qualified Algebra.Graph                               as DG
import qualified Algebra.Graph.Undirected                    as UG
import           Control.Monad                               (forM_, replicateM)
import           Data.Binary                                 (Binary)
import           Data.Set                                    (intersection,
                                                              isSubsetOf)
import qualified Data.Set                                    as S
import           GHC.Generics                                (Generic)
import           Type.Reflection                             (Typeable)


import           Control.Exception                           (assert)
import           LocalComputation.Inference.JoinTree         (Node (..),
                                                              baseJoinTree)
import qualified LocalComputation.Inference.JoinTree         as J
import qualified LocalComputation.Inference.JoinTree.Diagram as D
import           LocalComputation.Utils
import           LocalComputation.ValuationAlgebra

type InferredData v a = DG.Graph (Node (v a))

-- TODO: safely handle invalid queries?
answerQueries :: forall v a. (Show a, Valuation v, Ord a)
    => [Domain a]
    -> InferredData v a
    -> [v a]
answerQueries queryDomains results = map queryToAnswer queryDomains
    where
        queryToAnswer :: Domain a -> v a
        queryToAnswer d = project (unsafeFind (\n -> d `isSubsetOf` n.d) (DG.vertexList results)).v d

-- TODO safely handle invalid queries?
answerQuery :: forall v a. (Show a, Valuation v, Ord a)
    => Domain a
    -> InferredData v a
    -> v a
answerQuery q results = head $ answerQueries [q] results

answerQueriesM :: forall v a . (Show a, Serializable (v a), Valuation v, Ord a)
    => [v a]
    -> [Domain a]
    -> Process [v a]
answerQueriesM vs queryDomains = do
    results <- initializeNodes (baseJoinTree vs queryDomains)
    pure $ answerQueries queryDomains results

answerQueryM :: forall v a . (Show a, Serializable (v a), Valuation v, Ord a)
    => [v a]
    -> Domain a
    -> Process (v a)
answerQueryM vs q = do
    results <- initializeNodes (baseJoinTree vs [q])
    pure $ answerQuery q results

-- TODO: Right now visualises the after tree. We should have options for both i guess!
answerQueriesDrawGraphM :: forall v a . (Show a, Show (v a), Serializable (v a), Valuation v, Ord a)
    => FilePath
    -> [v a]
    -> [Domain a]
    -> Process [v a]
answerQueriesDrawGraphM filename vs queryDomains = do
    let tree = baseJoinTree vs queryDomains
    liftIO $ D.draw filename tree
    results <- initializeNodes tree
    -- liftIO $ D.draw filename results
    pure $ answerQueries queryDomains results

inference :: forall v a . (Show a, Serializable (v a), Valuation v, Ord a)
    => [v a]
    -> [Domain a]
    -> Process (InferredData v a)
inference vs queryDomains = initializeNodes (baseJoinTree vs queryDomains)

-- The base join tree must be transformed to an undirected graph.
-- While mailboxes should be connected up for each neighbour, this happens in the
-- 'initializeNodes' function which also handles starting the message passing.
shenoyJoinTree :: forall v a. (Show a, Valuation v, Ord a)
    => [v a]
    -> [Domain a]
    -> UG.Graph (Node (v a))
shenoyJoinTree vs queryDomains = UG.toUndirected (baseJoinTree vs queryDomains)

data NodeWithProcessId a = NodeWithProcessId { id :: ProcessId, node :: a } deriving (Generic, Binary)

-- Initializes all nodes in the join tree for message passing according to the Shenoy-Shafer algorithm.
initializeNodes :: forall v a. (Show a, Serializable (v a), Valuation v, Ord a)
    => DG.Graph (Node (v a))
    -> Process (DG.Graph (Node (v a)))
initializeNodes directed = do

    -- Initialize all nodes
    let undirected = UG.toUndirected directed
        vs         = UG.vertexList undirected
    (nodesWithPid, resultPorts) <- fmap unzip $ mapM initializeNodeAndMonitor vs

    -- Tell each node who it is and who it's neighbours are
    forM_ nodesWithPid $ \nodeWithPid -> do
        let neighbours = S.toList $ UG.neighbours nodeWithPid.node undirected
            neighboursWithPid = filter (\n -> n.node `elem` neighbours) nodesWithPid
        send nodeWithPid.id nodeWithPid
        send nodeWithPid.id neighboursWithPid

    -- Wait for normal termination
    _ <- forM_ nodesWithPid $ \_ -> do
        (ProcessMonitorNotification _ _ reasonForTermination) <- expect
        case reasonForTermination of
             DiedNormal      -> pure ()
             DiedException e -> error $ "Error - DiedException (" ++ e ++ ")"
             -- TODO: Does 'DiedUnknownId' indicate that the process died *before*
             -- we got a chance to wait on it?
             x               -> error $ "Error - " ++ show x

    -- All should be terminated - receive all messages
    newNodes <- mapM (\p -> fmap assertHasMessage $ receiveChanTimeout 0 p) resultPorts

    -- Construct graph from new nodes
    pure $ fmap (\oldNode -> unsafeFind (\newNode -> newNode.id == oldNode.id) newNodes) directed


    where
        initializeNodeAndMonitor :: Node (v a) -> Process (NodeWithProcessId (Node (v a)), ReceivePort (Node (v a)))
        initializeNodeAndMonitor node = do
            (sendFinalResult, receiveFinalResult) <- newChan

            i <- initializeNode sendFinalResult
            _ <- monitor i

            pure (NodeWithProcessId i node, receiveFinalResult)

assertHasMessage :: Maybe a -> a
assertHasMessage (Just x) = x
assertHasMessage Nothing = error "Error - a node terminated without sending a message on the result channel"

data Message a = Message {
          sender :: ProcessId
        , msg    :: a
    } deriving (Generic, Binary)

initializeNode :: forall v a. (
      Show a
    , Binary (v a), Typeable (v a)
    , Valuation v
    , Ord a
    )
    => SendPort (Node (v a))
    -> Process ProcessId
initializeNode resultPort = spawnLocal $ do
    this :: NodeWithProcessId (Node (v a)) <- expect
    neighbours :: [NodeWithProcessId (Node (v a))] <- expect

    --[[ Phase 1: Collect Phase ]]
    -- Wait for messages from all neighbours bar one
    (phase1Postbox, neighbourWhoDidntSend) <- receivePhaseOne neighbours

    -- Combine messages into new message, and send to the only neighbour we didn't receive a message from.
    sendMessage' phase1Postbox this neighbourWhoDidntSend

    --[[ Phase 2: Distribute Phase ]]
    -- Wait for response from neighbour we just sent a message to
    message :: Message (v a) <- expect

    let phase2Postbox = message : phase1Postbox
    assert (message.sender == neighbourWhoDidntSend.id) (pure ())

    -- Send out messages to remaining neighbours (which we now have enough information to send messages to)
    sequence_ $ map (\n -> sendMessage phase2Postbox this n)
                    (filterOut neighbourWhoDidntSend neighbours)

    --[[ Sending Results ]]
    -- Send result back to parent process
    let result = combines1 (this.node.v : map (.msg) phase2Postbox)
    assert (this.node.d == label result) (pure ())
    sendChan resultPort (J.node this.node.id result this.node.t)

    where
        filterOut :: NodeWithProcessId (Node (v a)) -> [NodeWithProcessId (Node (v a))] -> [NodeWithProcessId (Node (v a))]
        filterOut neighbour neighbours = filter (\n -> n.id /= neighbour.id) neighbours


-- | Sends a message from the given sender to the given receiver.
--
-- Sending a message consists of:
--  1. combining all messages in the sender's postbox that don't come from the recipient
--  2. combining this result with the sender's valuation
--  3. projecting the result to the intersection of the sender and recipient's domain
--  4. dispatching the resulting message to the recipient node.
sendMessage :: (Show a, Serializable (v a), Valuation v, Ord a)
    => [Message (v a)]
    -> NodeWithProcessId (Node (v a))
    -> NodeWithProcessId (Node (v a))
    -> Process ()
sendMessage postbox sender recipient = sendMessage' (filter (\msg -> msg.sender /= recipient.id) postbox)
                                                    sender
                                                    recipient

-- | Same as `sendMessage` except doesn't filter the given postbox for messages that don't come from the
-- recipient. Hence should only be used when it is known that none of the messages in the postbox come
-- from the recipient.
sendMessage' :: (Show a, Serializable (v a), Valuation v, Ord a)
    => [Message (v a)]
    -> NodeWithProcessId (Node (v a))
    -> NodeWithProcessId (Node (v a))
    -> Process ()
sendMessage' postbox sender recipient = send recipient.id msg
    where
        msg = Message sender.id (project (combines1 (sender.node.v : map (.msg) postbox))
                                         (intersection sender.node.d recipient.node.d))

-- | Receives messages from all neighbours but one, returning the neighbour that it never
-- received a message from.
receivePhaseOne :: Serializable (v a)
    => [NodeWithProcessId (Node (v a))]
    -> Process ([Message (v a)], NodeWithProcessId (Node (v a)))
receivePhaseOne [] = error "receivePhaseOne: Attempted to receive from no port."
receivePhaseOne neighbours = do

    postbox <- replicateM (length neighbours - 1) expect

    let senders = map (.sender) postbox
        neighbourWhoDidntSend = findAssertSingleMatch (\n -> n.id `notElem` senders) neighbours

    pure (postbox, neighbourWhoDidntSend)


------------------------------------------------------------------------------
-- Solution Construction                                                    --
------------------------------------------------------------------------------

-- configSet :: (Valuation v, Show a, Ord a)
--     => v a
--     -> Domain a
--     -> VariableArrangement v a b
--     -> Maybe (S.Set (VariableArrangement v a b))
-- configSet phi t x
--     | not $ S.isSubsetOf t (label phi) = Nothing
--     | otherwise = undefined



-- type SolutionSet v a = ConfigurationExtensionSet v a
--
-- -- | The configuration extension set.
-- --
-- -- This is detailed in page 294 of Marc Pouly's "Generic Inference". In short, this is an intermediate
-- -- product in a larger computation and is related to the set of variables that have not yet been assigned
-- -- values.
-- data ConfigurationExtensionSet v a = ConfigurationExtensionSet {
--           t   :: Domain a
--         , phi :: v a
--         , f   :: VariableArrangement a b -> S.Set (VariableArrangement a b)
--     }
--
-- instance (Valuation v, Ord a, Ord b, Show a, Show b) => HasField "s" (ConfigurationExtensionSet v a) (Domain a) where
--     getField w = label w.phi
--
-- -- | Compute all solutions.
-- --
-- -- Note this function does not require a complete run of the specifically the shenoy shafer architecture,
-- -- but rather any multi-query local computation architecture should suffice.
-- -- This algorithm is based off page 299 of Marc Pouly's "Generic Inference".
-- computeSolutions ::
--        InferredData v a
--     -> ConfigurationExtensionSet v a
-- computeSolutions = undefined
--
-- isValidConfigurationExtensionSet :: (Valuation v, Ord a, Ord b, Show a, Show b)
--     => ConfigurationExtensionSet v a
--     -> VariableArrangement a b
--     -> Bool
-- isValidConfigurationExtensionSet w x
--     -- Fits definition of configuration set from `t` to `s`
--     | not $ S.isSubsetOf w.t w.s = False
--     | not $ all elemOfOmegaSMinusT (w.f x) = False
--
--     where
--         elemOfOmegaSMinusT y = S.isSubsetOf (M.keysSet y) (S.difference w.s w.t)
--

--
-- instance (Valuation v, Ord a, Ord b, Show a, Show b) => HasField "s" (ConfigurationExtensionSet v a) (Domain a) where
--     getField w = label w.phi
--
-- -- TODO: Add the final property
--
-- isValidConfigurationExtensionSet :: (Valuation v, Ord a, Ord b, Show a, Show b)
--     => ConfigurationExtensionSet v a
--     -> VariableArrangement a b
--     -> Bool
-- isValidConfigurationExtensionSet w x
--     -- Fits definition of configuration set from `t` to `s`
--     | not $ S.isSubsetOf w.t w.s = False
--     | not $ all elemOfOmegaSMinusT (w.f x) = False
--     | otherwise = True
--
--     where
--         elemOfOmegaSMinusT y = S.isSubsetOf (Map.keysSet y) (S.difference w.s w.t)
--
-- | Compute all solutions.
--
-- Note this function does not require a complete run of the specifically the shenoy shafer architecture,
-- but rather any multi-query local computation architecture should suffice.
-- This algorithm is based off page 299 of Marc Pouly's "Generic Inference".
-- computeSolutions ::
--        [(Domain a, v a)]
--     -> ConfigurationExtensionSet v a
-- computeSolutions = undefined
