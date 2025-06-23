{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}

module ShenoyShafer
    (
        initializeNodes
        , shenoyJoinTree
    )
where

-- Cloud Haskell
import Control.Concurrent (threadDelay)
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Distributed.Process.Serializable
import Network.Transport.TCP

import qualified Algebra.Graph
import Algebra.Graph.Undirected
import qualified Data.List
import Control.Monad (replicateM)
import Control.Monad.Extra (concatMapM)
import Data.Tuple.Extra (snd3)
import Type.Reflection (Typeable)
import Data.Binary (Binary)
import Data.Char (chr)
import Data.Time (getCurrentTime, utctDayTime, UTCTime)
import Text.Printf (printf)
import Data.Set (intersection)


import ValuationAlgebra
import JoinTree
import Utils




data ShenoyShaferNode v a b = ShenoyShaferNode Integer (Domain a) (v a b)

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
    -- show (ShenoyShaferNode i d v) = "--------------------"     ++ "\n"
    --                         ++ "[Index]: "      ++ show i ++ "\n"
    --                         ++ "[Domain]: "     ++ show d ++ "\n"
    --                         -- ++ "[Valuation]: "  ++ show v ++ "\n"
    --                         -- ++ "--------------------"     ++ "\n"

answerQueries :: forall v a b.
    [Domain a]
    -> [v a b]
    -> [v a b]
answerQueries = undefined

-- The base join tree must be transformed to an undirected graph.
-- While mailboxes should be connected up for each neighbour, this happens in the
-- 'initializeNodes' function which also handles starting the message passing.
shenoyJoinTree :: forall v a b. (Valuation v, Ord a)
    => [v a b]
    -> [Domain a]
    -> Graph (ShenoyShaferNode v a b)
shenoyJoinTree vs qs = toUndirected (baseJoinTree vs qs)

-- Initializes all nodes in the join tree for message passing according to the Shenoy-Shafer algorithm.
-- TODO probably want to take in queries as a parameter, and return Process ([v a b]) where [v a b] is a list of answers.
--      This will probably involve passing a 'server' process id to each node (or a dedicated pipe to talk to the server),
--      as we need a way of getting the 'results' of the query nodes.
initializeNodes :: forall n v a b. (Node n, Serializable (v a b), Valuation v, Ord (n v a b), Ord a, Ord b)
    => Graph (n v a b)
    -> Process ()
initializeNodes graph = do
    ports <- portsM
    mapM_ (initializeNode' ports) (vertexList graph)

    where

        portsForEdge :: (n v a b, n v a b)
                    -> Process [(n v a b, Domain a, SendPort (v a b), ReceivePort (v a b))]
        portsForEdge (x, y) = do
            (xSendPort, yReceivePort) <- newChan
            (ySendPort, xReceivePort) <- newChan

            return [(x, getDomain y, xSendPort, xReceivePort), (y, getDomain x, ySendPort, yReceivePort)]

        portsM :: Process [(n v a b, Domain a, SendPort (v a b), ReceivePort (v a b))]
        portsM = concatMapM portsForEdge (edgeList graph)

        portsForVertex :: n v a b
            -> [(n v a b, Domain a, SendPort (v a b), ReceivePort (v a b))]
            -> [(Domain a, SendPort (v a b), ReceivePort (v a b))]
        portsForVertex node mapping = map (\(_, d, s, r) -> (d, s, r)) $ filter (\(n, _, _, _) -> n == node) mapping

        initializeNode' :: [(n v a b, Domain a, SendPort (v a b), ReceivePort (v a b))]
            -> n v a b
            -> Process ProcessId
        initializeNode' ports node = initializeNode node (portsForVertex node ports)


type PortIdentifier = Integer

initializeNode :: forall n v a b. (Node n, Binary (v a b), Typeable (v a b), Valuation v, Ord a, Ord b)
    => n v a b
    -> [(Domain a, SendPort (v a b), ReceivePort (v a b))]
    -> Process ProcessId
initializeNode node ports = spawnLocal $ do

    -- COLLECT PHASE

    -- Wait for messages from (length ports - 1) ports
    (initialMessages, unusedPortId) <- receivePhaseOne receivePorts

    collectTime <- liftIO $ formatTimeNicely <$> getCurrentTime
    liftIO $ putStrLn $ collectTime ++ " [COLLECT]    " ++ show (chr $ fromInteger $ nodeId node) ++ " received from "
                        ++ show (length initialMessages) ++ " node(s). Sending..."

    -- Combine messages into new message, and send to the only port we didn't receive a message from.
    let unusedPort = idToPort unusedPortId
    sendMessage (getValuation node : map snd initialMessages) (getDomain node) (snd4 unusedPort) (thd4 unusedPort)

    -- DISTRIBUTE PHASE

    -- Wait for response from port we just sent a message to
    message <- receiveChan (fth4 unusedPort)

    distributeTime <- liftIO $ formatTimeNicely <$> getCurrentTime
    liftIO $ putStrLn $ distributeTime ++ " [DISTRIBUTE] " ++ show (chr $ fromInteger $ nodeId node) ++ " received. "
                        ++ "Sending to " ++ show (length initialMessages) ++ " node(s)..."

    -- Combine this message with the old ones
    let allMessages = (unusedPortId, message) : initialMessages

    sequence_ $ map (sendPhaseTwo allMessages) (allPortsExcept unusedPortId)

    where
        ports' :: [(PortIdentifier, Domain a, SendPort (v a b), ReceivePort (v a b))]
        ports' = zipWith (\x (d, s, r) -> (x, d, s, r)) [0..] ports

        receivePorts :: [(PortIdentifier, ReceivePort (v a b))]
        receivePorts = map (\(x, _, _, r) -> (x, r)) ports'

        idToPort :: PortIdentifier -> (PortIdentifier, Domain a, SendPort (v a b), ReceivePort (v a b))
        idToPort p = unsafeFind (\(x, _, _, _) -> x == p) ports'

        allPortsExcept :: PortIdentifier -> [(PortIdentifier, Domain a, SendPort (v a b))]
        allPortsExcept used = map (\(i, d, s, _) -> (i, d, s)) $ filter (\(x, _, _, _) -> x /= used) ports'

        sendPhaseTwo :: (Serializable (v a b))
            => [(PortIdentifier, v a b)]
            -> (PortIdentifier, Domain a, SendPort (v a b))
            -> Process ()
        sendPhaseTwo allMessages (i, d, s) = sendMessage (getValuation node : (map snd $ filter (\(i', _) -> i' == i) allMessages)) (getDomain node) d s

sendMessage :: (Serializable (v a b), Valuation v, Ord a, Ord b)
    => [v a b]
    -> Domain a
    -> Domain a
    -> SendPort (v a b)
    -> Process ()
sendMessage msgsToCombine nodeDomain recipientDomain sendPort = sendChan sendPort (project (combines msgsToCombine) (intersection nodeDomain recipientDomain))


-- Receives messages from all ports but one, returning the port that it never
-- received a message from.
receivePhaseOne :: Serializable a
    => [(PortIdentifier, ReceivePort a)]
    -> Process ([(PortIdentifier, a)], PortIdentifier)
receivePhaseOne [] = error "receivePhaseOne: Attempted to receive from no port."
receivePhaseOne [p] = return ([], fst p)
receivePhaseOne ps = do
    (message, ps') <- receiveOnce ps
    (messages, unusedPort) <- receivePhaseOne ps'
    return ((message : messages), unusedPort)

-- This non-blocking approach may or may not be less efficent than
-- cloud haskells merged ports implementation - however we cannot
-- use the merged ports as we don't know which port returned the value.
-- One way to get around this could be to send an indentifier with
-- the value through the port.
receiveOnce :: Serializable a
    => [(PortIdentifier, ReceivePort a)]
    -> Process ((PortIdentifier, a), [(PortIdentifier, ReceivePort a)])
receiveOnce [] = error "receiveOnce: Attempted to receive from no port."
receiveOnce ((pIndex, p):ps) = do
    x <- receiveChanTimeout 0 p
    case x of
         (Just message) -> return ((pIndex, message), ps)
         Nothing -> receiveOnce (ps ++ [(pIndex, p)])



-- can be moved to utils file
unsafeFind :: Foldable t => (a -> Bool) -> t a -> a
unsafeFind p xs
    | (Just y) <- Data.List.find p xs = y
    | otherwise = error "unsafeFind found nothing"

formatTimeNicely :: UTCTime -> String
formatTimeNicely time = printf "[%02d:%02d:%02d]" hours minutes seconds

    where
        secondsPastMidnight :: Integer
        secondsPastMidnight = floor $ utctDayTime time

        seconds = secondsPastMidnight `mod` 60
        minutes = secondsPastMidnight `div` 60 `mod` 60
        hours = secondsPastMidnight `div` 60 `div` 60 `mod` 60


