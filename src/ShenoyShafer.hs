{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ShenoyShafer
    (
        initializeNodes
        , initializeNodes3
        , shenoyJoinTree
    )
where

-- Cloud Haskell
import Control.Concurrent (threadDelay)
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Network.Transport.TCP

import qualified Algebra.Graph
import Algebra.Graph.Undirected
import qualified Data.List


import ValuationAlgebra
import JoinTree




data ShenoyShaferNode v a b = ShenoyShaferNode Integer (Domain a) (v a b) [(SendPort (v a b), ReceivePort (v a b))]

instance Node ShenoyShaferNode where
    collect = undefined

    getValuation (ShenoyShaferNode _ _ v _) = v

    getDomain (ShenoyShaferNode _ d _ _) = d

    create i d v = ShenoyShaferNode i d v []

    nodeId (ShenoyShaferNode i _ _ _) = i

instance Eq (ShenoyShaferNode v a b) where
    x == y = nodeId x == nodeId y

instance Ord (ShenoyShaferNode v a b) where
    x <= y = nodeId x <= nodeId y

-- Could put implementation inside the Node typeclass and then just call it from in here
instance (Show (v a b), Show a) => Show (ShenoyShaferNode v a b) where
    show (ShenoyShaferNode i d v _) = "--------------------"     ++ "\n"
                            ++ "[Index]: "      ++ show i ++ "\n"
                            ++ "[Domain]: "     ++ show d ++ "\n"
                            ++ "[Valuation]: "  ++ show v ++ "\n"
                            ++ "--------------------"     ++ "\n"


answerQueries :: forall v a b. (Valuation v, Eq a)
    => [Domain a]
    -> [v a b]
    -> v a b
answerQueries = undefined


-- The base join tree must be transformed to an undirected graph, and mailboxes
-- connected up for each neighbour. Mailboxes are represented by channels (AKA ports)
-- so creating the join tree creates a process?
shenoyJoinTree :: forall v a b. (Valuation v, Eq a)
    => [v a b]
    -> [Domain a]
    -> Graph (ShenoyShaferNode v a b)
shenoyJoinTree vs qs = undirectedGraph
    where
        undirectedGraph :: Graph (ShenoyShaferNode v a b)
        undirectedGraph = toUndirected (baseJoinTree vs qs)

        -- inference :: Process ()
        -- inference = do
        --     mapM f (adjacencyList undirectedGraph)
        --
        --     where
        --         f :: (ShenoyShaferNode v a b, [ShenoyShaferNode v a b]) -> Process ()
        --         f (node, neighbours) = do
        --             mapM g neighbours
        --
        --             where
        --                 g :: ShenoyShaferNode v a b -> Process ()
        --                 g neighbour = do
        --                     (s, r) <- newChan
                            


initializeNodes :: forall v a b. (Valuation v, Eq a)
    => Graph (ShenoyShaferNode v a b)
    -> Graph (Process ProcessId)
initializeNodes = foldg empty f overlay g
    where
        -- maps a node to a process creator
        f :: ShenoyShaferNode v a b -> Graph (Process ProcessId)
        f v = vertex $ do
            spawnLocal $ do
                anId <- expect
                liftIO $ putStrLn $ "Vertex " ++ show (nodeId v) ++ " Received: " ++ show (anId :: ProcessId)
                anId2 <- expect
                liftIO $ putStrLn $ "!2! Vertex " ++ show (nodeId v) ++ " Received: " ++ show (anId2 :: ProcessId)

        g :: Graph (Process ProcessId) -> Graph (Process ProcessId) -> Graph (Process ProcessId)
        g xSpawners ySpawners = do
            -- x and y are processes that spawn a process
            xSpawner <- xSpawners
            ySpawner <- ySpawners

            connect
                (
                vertex $ do
                    -- extends the xSpawner to send 

                    -- creates a process, gets its id
                    xId <- xSpawner
                    -- creates a process, gets its id
                    -- doesnt this create another y because this is called in the other vertex too...
                    yId <- ySpawner
                    -- sends the pid of the process handling node x to the
                    -- process handling node y
                    send yId xId

                    -- returns the pid of the process handling node x
                    return xId
                )
                (
                vertex $ do
                    xId <- xSpawner
                    yId <- ySpawner
                    send xId yId
                    return yId
                )

-- If you did the channel approach what you would want is to immediately spawn
-- a process which creates all the necessary channels and then spawns processes
-- one by one, giving them the channels they need as a part of their parameters.
initializeNodes3 :: forall v a b. (Valuation v, Eq a)
    => Graph (ShenoyShaferNode v a b)
    -> Process ()
initializeNodes3 graph = do

    processIds <- processIdsM
    mapM_ connectNodes (edgesAsProcessIds processIds)

    where
        processes :: [(ShenoyShaferNode v a b, Process ProcessId)]
        processes = map f (vertexList graph)
            where
                f :: ShenoyShaferNode v a b -> (ShenoyShaferNode v a b, Process ProcessId)
                f node = (node,
                          spawnLocal $ do
                              receiveAllNeighboursPids node

                              liftIO $ putStrLn $ "Vertex " ++ show (nodeId node) ++ " ready to start!"
                    )

        receiveAllNeighboursPids :: ShenoyShaferNode v a b -> Process ()
        receiveAllNeighboursPids node = do
            mapM_ f (neighbours node graph)

            where
                f :: ShenoyShaferNode v a b -> Process ProcessId
                f _ = do
                    anId <- expect
                    liftIO $ putStrLn $ "Vertex " ++ show (nodeId node) ++ " Received: " ++ show (anId :: ProcessId)
                    return anId


        processIdsM :: Process [(ShenoyShaferNode v a b, ProcessId)]
        processIdsM = mapM g processes
            where
                g :: (ShenoyShaferNode v a b, Process ProcessId) -> Process (ShenoyShaferNode v a b, ProcessId)
                g (v, p) = do
                    pid <- p
                    return (v, pid)

        edgesAsProcessIds :: [(ShenoyShaferNode v a b, ProcessId)] -> [(ProcessId, ProcessId)]
        edgesAsProcessIds mapping = map f (edgeList graph)
            where
                f :: (ShenoyShaferNode v a b, ShenoyShaferNode v a b) -> (ProcessId, ProcessId)
                f (x, y) = (nodeToId x, nodeToId y)

                nodeToId :: ShenoyShaferNode v a b -> ProcessId
                nodeToId node = snd $ unsafeFind (\(n, _) -> n == node) mapping

        connectNodes :: (ProcessId, ProcessId) -> Process ()
        connectNodes (x, y) = do
            send x y
            send y x

-- can be moved to utils file
unsafeFind :: Foldable t => (a -> Bool) -> t a -> a
unsafeFind p xs
    | (Just y) <- Data.List.find p xs = y
    | otherwise = error "unsafeFind found nothing"

-- initializeNodes2 :: forall v a b. (Valuation v, Eq a)
--     => Graph (ShenoyShaferNode v a b)
--     -> [Process ProcessId]
-- initializeNodes2 = foldg [] f (++) g
--     where
--         -- maps a node to a process creator
--         f :: ShenoyShaferNode v a b -> [Process ProcessId]
--         f v = [
--             spawnLocal $ do
--                 anId <- expect
--                 liftIO $ putStrLn $ "Vertex " ++ show (nodeId v) ++ " Received: " ++ show (anId :: ProcessId)
--                 anId2 <- expect
--                 liftIO $ putStrLn $ "!2! Vertex " ++ show (nodeId v) ++ " Received: " ++ show (anId2 :: ProcessId)
--               ]
-- 
-- 
--         g :: [Process ProcessId] -> [Process ProcessId] -> [Process ProcessId]
--         g xSpawners ySpawners = do
--             -- x and y are processes that spawn a process
--             xSpawner <- xSpawners
--             ySpawner <- ySpawners
-- 
--             connect
--                 (
--                 vertex $ do
--                     -- extends the xSpawner to send 
-- 
--                     -- creates a process, gets its id
--                     xId <- xSpawner
--                     -- creates a process, gets its id
--                     -- doesnt this create another y because this is called in the other vertex too...
--                     yId <- ySpawner
--                     -- sends the pid of the process handling node x to the
--                     -- process handling node y
--                     send yId xId
-- 
--                     -- returns the pid of the process handling node x
--                     return xId
--                 )
--                 (
--                 vertex $ do
--                     xId <- xSpawner
--                     yId <- ySpawner
--                     send xId yId
--                     return yId
--                 )



            -- OR get neighbours from graph and expect that many times...


-- node :: ShenoyShaferNode v a b -> [ProcessId] -> Process ()
-- node v (n:neighbours) = do
-- 
--     (valuation, senderId) <- expect
-- 
--     send 



    
-- to spawn a node we need the process id of the node it is going to send a message to in the collect phase.
--


    














