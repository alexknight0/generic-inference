{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LocalComputation.Inference.ShenoyShafer
    (
        initializeNodes
        , shenoyJoinTree
        , answerQueriesM, answerQueryM
        , answerQueries, answerQuery
        , inference
        , InferredData
    )
where

import           Control.Distributed.Process              hiding (Message)
import           Control.Distributed.Process.Serializable

import           Algebra.Graph.Undirected                 hiding (neighbours)
import qualified Algebra.Graph.Undirected                 as G
import           Control.Monad                            (forM_, replicateM)
import           Data.Binary                              (Binary)
import qualified Data.Map.Lazy                            as M
import           Data.Set                                 (intersection,
                                                           isSubsetOf)
import qualified Data.Set                                 as S
import           Type.Reflection                          (Typeable)

-- Typeclasses
import           GHC.Generics                             (Generic)
import           GHC.Records                              (HasField, getField)


import           Control.Exception                        (assert)
import           LocalComputation.Inference.JoinTree
import           LocalComputation.Utils
import           LocalComputation.ValuationAlgebra

data ShenoyShaferNode v a b = ShenoyShaferNode Integer (Domain a) (v a b) deriving (Generic, Binary)

instance Node ShenoyShaferNode where
    collect = undefined

    getValuation (ShenoyShaferNode _ _ v) = v

    getDomain (ShenoyShaferNode _ d _) = d

    create i d v = ShenoyShaferNode i d v

    nodeId (ShenoyShaferNode i _ _) = i

instance Eq (ShenoyShaferNode v a b) where
    x == y = nodeId x == nodeId y

instance Ord (ShenoyShaferNode v a b) where
    x <= y = nodeId x <= nodeId y

-- TODO: Could put implementation inside the Node typeclass and then just call it from in here
instance (Show (v a b), Show a) => Show (ShenoyShaferNode v a b) where
    show (ShenoyShaferNode i d _) = show (i, d)

type InferredData v a b = [(Domain a, v a b)]

-- TODO: safely handle invalid queries?
answerQueries :: forall v a b. (Show a, Show b, Valuation v, Ord a, Ord b)
    => [Domain a]
    -> InferredData v a b
    -> [v a b]
answerQueries queryDomains results = map queryToAnswer queryDomains
    where
        queryToAnswer :: Domain a -> v a b
        queryToAnswer d = project (snd $ unsafeFind (\(d', _) -> d `isSubsetOf` d') results) d

-- TODO safely handle invalid queries?
answerQuery :: forall v a b. (Show a, Show b, Valuation v, Ord a, Ord b)
    => Domain a
    -> InferredData v a b
    -> v a b
answerQuery q results = head $ answerQueries [q] results

answerQueriesM :: forall v a b . (Show a, Show b, Typeable v, Typeable b, Serializable (v a b), Serializable a, Valuation v, Ord a, Ord b)
    => [v a b]
    -> [Domain a]
    -> Process [v a b]
answerQueriesM vs queryDomains = do
    results <- initializeNodes (shenoyJoinTree vs queryDomains)
    pure $ answerQueries queryDomains results

answerQueryM :: forall v a b . (Show a, Show b, Typeable v, Typeable b, Serializable (v a b), Serializable a, Valuation v, Ord a, Ord b)
    => [v a b]
    -> Domain a
    -> Process (v a b)
answerQueryM vs q = do
    results <- initializeNodes (shenoyJoinTree vs [q])
    pure $ answerQuery q results

inference :: forall v a b . (Show a, Show b, Typeable v, Typeable b, Serializable (v a b), Serializable a, Valuation v, Ord a, Ord b)
    => [v a b]
    -> [Domain a]
    -> Process (InferredData v a b)
inference vs queryDomains = initializeNodes (shenoyJoinTree vs queryDomains)

-- The base join tree must be transformed to an undirected graph.
-- While mailboxes should be connected up for each neighbour, this happens in the
-- 'initializeNodes' function which also handles starting the message passing.
shenoyJoinTree :: forall v a b. (Show a, Show b, Valuation v, Ord a, Ord b)
    => [v a b]
    -> [Domain a]
    -> Graph (ShenoyShaferNode v a b)
shenoyJoinTree vs queryDomains = toUndirected (baseJoinTree vs queryDomains)

data NodeWithProcessId a = NodeWithProcessId { id :: ProcessId, node :: a } deriving (Generic, Binary)

-- Initializes all nodes in the join tree for message passing according to the Shenoy-Shafer algorithm.
initializeNodes :: forall n v a b. (Show a, Show b, Typeable n, Typeable v, Typeable b, Binary (n v a b), Serializable (v a b), Serializable a, Valuation v, Ord (n v a b), Ord a, Ord b)
    => Graph (n v a b)
    -> Process ([(Domain a, v a b)])
initializeNodes graph = do

    -- Initialize all nodes
    let vs = vertexList graph
    (nodesWithPid, resultPorts) <- fmap unzip $ mapM initializeNodeAndMonitor vs

    -- Tell each node who it is and who it's neighbours are
    forM_ nodesWithPid $ \nodeWithPid -> do
        let neighbours = S.toList $ G.neighbours nodeWithPid.node graph
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
    mapM (\p -> fmap assertHasMessage $ receiveChanTimeout 0 p) resultPorts

    where
        initializeNodeAndMonitor :: n v a b -> Process (NodeWithProcessId (n v a b), ReceivePort (Domain a, v a b))
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

-- TODO : rename 'node' to 'this' and both 'messages' constants to 'postbox'
initializeNode :: forall v a b. (Show a, Show b, Binary (v a b), Binary a, Typeable v, Typeable b, Typeable (v a b), Typeable a, Valuation v, Ord a, Ord b)
    => SendPort (Domain a, v a b)
    -> Process ProcessId
initializeNode resultPort = spawnLocal $ do
    this :: NodeWithProcessId (ShenoyShaferNode v a b) <- expect
    neighbours :: [NodeWithProcessId (ShenoyShaferNode v a b)] <- expect

    --[[ Phase 1: Collect Phase ]]
    -- Wait for messages from all neighbours bar one
    (phase1Postbox, neighbourWhoDidntSend) <- receivePhaseOne neighbours

    -- Combine messages into new message, and send to the only neighbour we didn't receive a message from.
    sendMessage' phase1Postbox this neighbourWhoDidntSend

    --[[ Phase 2: Distribute Phase ]]
    -- Wait for response from neighbour we just sent a message to
    message :: Message (v a b) <- expect

    let phase2Postbox = message : phase1Postbox
    assert (message.sender == neighbourWhoDidntSend.id) (pure ())

    -- Send out messages to remaining neighbours (which we now have enough information to send messages to)
    sequence_ $ map (\n -> sendMessage phase2Postbox this n)
                    (filterOut neighbourWhoDidntSend neighbours)

    --[[ Sending Results ]]
    -- Send result back to parent process
    sendChan resultPort (getDomain this.node, combines1 (getValuation this.node : map (.msg) phase2Postbox))

    where
        filterOut :: NodeWithProcessId (n v a b) -> [NodeWithProcessId (n v a b)] -> [NodeWithProcessId (n v a b)]
        filterOut neighbour neighbours = filter (\n -> n.id /= neighbour.id) neighbours

-- | Sends a message from the given sender to the given receiver.
--
-- Sending a message consists of:
--  1. combining all messages in the sender's postbox that don't come from the recipient
--  2. combining this result with the sender's valuation
--  3. projecting the result to the intersection of the sender and recipient's domain
--  4. dispatching the resulting message to the recipient node.
sendMessage :: (Show a, Show b, Serializable (v a b), Node n, Valuation v, Ord a, Ord b)
    => [Message (v a b)]
    -> NodeWithProcessId (n v a b)
    -> NodeWithProcessId (n v a b)
    -> Process ()
sendMessage postbox sender recipient = sendMessage' (filter (\msg -> msg.sender /= recipient.id) postbox)
                                                    sender
                                                    recipient

-- | Same as `sendMessage` except doesn't filter the given postbox for messages that don't come from the
-- recipient. Hence should only be used when it is known that none of the messages in the postbox come
-- from the recipient.
sendMessage' :: (Show a, Show b, Serializable (v a b), Node n, Valuation v, Ord a, Ord b)
    => [Message (v a b)]
    -> NodeWithProcessId (n v a b)
    -> NodeWithProcessId (n v a b)
    -> Process ()
sendMessage' postbox sender recipient = send recipient.id msg
    where
        msg = Message sender.id (project (combines1 (getValuation (sender.node) : map (.msg) postbox))
                                         (intersection (getDomain sender.node) (getDomain recipient.node)))

-- | Receives messages from all neighbours but one, returning the neighbour that it never
-- received a message from.
receivePhaseOne :: Serializable (v a b)
    => [NodeWithProcessId (n v a b)]
    -> Process ([Message (v a b)], NodeWithProcessId (n v a b))
receivePhaseOne [] = error "receivePhaseOne: Attempted to receive from no port."
receivePhaseOne neighbours = do

    postbox <- replicateM (length neighbours - 1) expect

    let senders = map (.sender) postbox
        neighbourWhoDidntSend = findAssertSingleMatch (\n -> n.id `notElem` senders) neighbours

    pure (postbox, neighbourWhoDidntSend)


data ConfigurationExtensionSet v a b = ConfigurationExtensionSet {
          t   :: Domain a
        , phi :: v a b
        , f   :: VariableArrangement a b -> S.Set (VariableArrangement a b)
    }

instance (Valuation v, Ord a, Ord b, Show a, Show b) => HasField "s" (ConfigurationExtensionSet v a b) (Domain a) where
    getField w = label w.phi

-- | Compute all solutions.
--
-- Note this function does not require a complete run of the specifically the shenoy shafer architecture,
-- but rather any multi-query local computation architecture should suffice.
-- This algorithm is based off page 299 of Marc Pouly's "Generic Inference".
computeSolutions ::
       InferredData v a b
    -> ConfigurationExtensionSet v a b
computeSolutions = undefined

isValidConfigurationExtensionSet :: (Valuation v, Ord a, Ord b, Show a, Show b)
    => ConfigurationExtensionSet v a b
    -> VariableArrangement a b
    -> Bool
isValidConfigurationExtensionSet w x
    -- Fits definition of configuration set from `t` to `s`
    | not $ S.isSubsetOf w.t w.s = False
    | not $ all elemOfOmegaSMinusT (w.f x) = False

    where
        elemOfOmegaSMinusT y = S.isSubsetOf (M.keysSet y) (S.difference w.s w.t)



-- type ConfigurationExtensionSet v a b =
--        v a b
--     -> Domain a
--     -> VariableArrangement a b
--     -> S.Set (VariableArrangement a b)
