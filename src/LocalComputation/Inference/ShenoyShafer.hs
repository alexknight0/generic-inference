{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
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

-- Cloud Haskell
import           Control.Concurrent                       (threadDelay)
import           Control.Distributed.Process              hiding (Message)
import           Control.Distributed.Process.Node
import           Control.Distributed.Process.Serializable
import           Network.Transport.TCP

import qualified Algebra.Graph
import           Algebra.Graph.Undirected                 hiding (neighbours)
import qualified Algebra.Graph.Undirected                 as G
import           Control.Monad                            (forM_, mzero,
                                                           replicateM)
import           Control.Monad.Extra                      (concatMapM)
import           Data.Binary                              (Binary)
import           Data.Char                                (chr)
import qualified Data.List
import qualified Data.Map                                 as M
import           Data.Set                                 (intersection,
                                                           isSubsetOf)
import qualified Data.Set                                 as S
import           Data.Time                                (UTCTime,
                                                           getCurrentTime,
                                                           utctDayTime)
import           Data.Tuple.Extra                         (snd3)
import           Text.Printf                              (printf)
import           Type.Reflection                          (Typeable)

-- Typeclasses
import           Control.DeepSeq                          (NFData)
import           Data.Binary                              (Binary)
import qualified Data.Hashable                            as H
import           GHC.Generics                             (Generic)

import           Control.Exception                        (assert, evaluate)
import           Debug.Trace                              (trace)
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

-- Could put implementation inside the Node typeclass and then just call it from in here
instance (Show (v a b), Show a) => Show (ShenoyShaferNode v a b) where
    show (ShenoyShaferNode i d _) = show (i, d)

type InferredData v a b = [(Domain a, v a b)]


-- TODO safely handle invalid queries?
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
    _ <- replicateM (length vs) $ do
        (ProcessMonitorNotification _ _ reasonForTermination) <- expect
        case reasonForTermination of
             DiedNormal      -> pure ()
             DiedException e -> error $ "Error - DiedException (" ++ e ++ ")"
             -- TODO: Does 'DiedUnknownId' indicate that the process died *before*
             -- we got a chance to wait on it?
             x               -> error $ "Error - " ++ show x

    -- Receive all messages
    mapM (\p -> fmap assertHasMessage $ receiveChanTimeout 0 p) resultPorts

    where
        -- portsForEdge :: (n v a b, n v a b)
        --             -> Process [PortWithNode n v a b]
        -- portsForEdge (x, y) = do
        --     (xSendPort, yReceivePort) <- newChan
        --     (ySendPort, xReceivePort) <- newChan

        --     return [PortWithNode x (Port (getDomain y) xSendPort xReceivePort),
        --             PortWithNode y (Port (getDomain x) ySendPort yReceivePort)]

        -- portsM :: Process [PortWithNode n v a b]
        -- portsM = concatMapM portsForEdge (edgeList graph)

        -- portsForVertex :: n v a b
        --     -> [PortWithNode n v a b]
        --     -> [Port v a b]
        -- portsForVertex node mapping = map (.port) $ filter (\n -> n.node == node) mapping

        -- TODO: we initialize each node, then send each node a list of all of its neighbours.

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
    node :: NodeWithProcessId (ShenoyShaferNode v a b) <- expect
    neighbours :: [NodeWithProcessId (ShenoyShaferNode v a b)] <- expect

    -- COLLECT PHASE

    -- Wait for messages from (length ports - 1) ports
    (initialMessages, neighbourWhoDidntSend) <- receivePhaseOne neighbours

    -- Combine messages into new message, and send to the only port we didn't receive a message from.
    sendMessage initialMessages node neighbourWhoDidntSend

    -- DISTRIBUTE PHASE

    -- Wait for response from port we just sent a message to
    message :: Message (v a b) <- expect

    -- Combine this message with the old ones
    let allMessages = message : initialMessages
    assert (message.sender == neighbourWhoDidntSend.id) (pure ())

    -- Send out messages to remaining nodes (which we now have enough information to send messages to)
    sequence_ $ map (sendPhaseTwo allMessages node) (filter (\n -> n.id /= neighbourWhoDidntSend.id) neighbours)

    -- QUERY ANSWERING

    -- Send result back to parent process
    sendChan resultPort (getDomain node.node, combines (getValuation node.node : map (.msg) allMessages))

    where
        allMessagesExceptMyOwn :: NodeWithProcessId (n v a b) -> [Message (v a b)] -> [Message (v a b)]
        allMessagesExceptMyOwn n msgs = filter (\msg -> msg.sender /= n.id) msgs

        sendPhaseTwo :: (Serializable (v a b), Node n)
            => [Message (v a b)]
            -> NodeWithProcessId (n v a b)
            -> NodeWithProcessId (n v a b)
            -> Process ()
        sendPhaseTwo allMessages sender recipient = sendMessage (allMessagesExceptMyOwn recipient allMessages) sender recipient

-- TODO: rename combines combines1
sendMessage :: (Show a, Show b, Serializable (v a b), Node n, Valuation v, Ord a, Ord b)
    => [Message (v a b)]
    -> NodeWithProcessId (n v a b)
    -> NodeWithProcessId (n v a b)
    -> Process ()
sendMessage msgsToCombine sender recipient = send recipient.id msg
    where
        -- TODO: This is not sufficent. We NEED the id of the receive port. I believe the sender ID has no bearing as the sender is a different node with a completely seperate port id setup.
        -- Well then dont we have NO WAY o fgetting the receiving id - no, but we need to thread it through from port creation?
        msg = Message sender.id (project (combines (getValuation (sender.node) : map (.msg) msgsToCombine)) (intersection (getDomain sender.node) (getDomain recipient.node)))

-- Receives messages from all ports but one, returning the port that it never
-- received a message from.
receivePhaseOne :: Serializable (v a b)
    => [NodeWithProcessId (n v a b)]
    -> Process ([Message (v a b)], NodeWithProcessId (n v a b))
receivePhaseOne [] = error "receivePhaseOne: Attempted to receive from no port."
receivePhaseOne neighbours = do

    msgs <- replicateM (length neighbours - 1) expect

    let senders = map (.sender) msgs
        unused = findAssertSingleMatch (\n -> n.id `notElem` senders) neighbours

    pure (msgs, unused)

-- receivePhaseOne [p] = return ([], p.id)
-- receivePhaseOne ps = do
--     (message, ps') <- receiveOnce ps
--     (messages, unusedPort) <- receivePhaseOne ps'
--     return ((message : messages), unusedPort)

-- This non-blocking approach may or may not be less efficent than
-- cloud haskells merged ports implementation - however we cannot
-- use the merged ports as we don't know which port returned the value.
-- One way to get around this could be to send an indentifier with
-- the value through the port.
-- receiveOnce :: Serializable (v a b)
--     => [PortWithId v a b]
--     -> Process ((PortIdentifier, v a b), [PortWithId v a b])
-- receiveOnce [] = error "receiveOnce: Attempted to receive from no port."
-- receiveOnce (p:ps) = do
--     x <- receiveChanTimeout 0 p.port.receive
--     case x of
--          (Just message) -> return ((p.id, message), ps)
--          Nothing        -> receiveOnce (ps ++ [p])



-- can be moved to utils file
formatTimeNicely :: UTCTime -> String
formatTimeNicely time = printf "[%02d:%02d:%02d]" hours minutes seconds

    where
        secondsPastMidnight :: Integer
        secondsPastMidnight = floor $ utctDayTime time

        seconds = secondsPastMidnight `mod` 60
        minutes = secondsPastMidnight `div` 60 `mod` 60
        hours = secondsPastMidnight `div` 60 `div` 60 `mod` 60


