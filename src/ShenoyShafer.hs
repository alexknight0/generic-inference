{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import Network.Transport.TCP

import qualified Algebra.Graph
import Algebra.Graph.Undirected


import ValuationAlgebra
import JoinTree
import Debug.Trace



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
        f :: ShenoyShaferNode v a b -> Graph (Process ProcessId)
        f v = vertex $ do
            spawnLocal $ do
                anId <- expect
                trace ("Received: " ++ show (anId :: ProcessId)) $ return ()

        g :: Graph (Process ProcessId) -> Graph (Process ProcessId) -> Graph (Process ProcessId)
        g xs ys = do
            x <- xs
            y <- ys

            connect
                (
                vertex $ do
                    xId <- x
                    yId <- y
                    send yId xId
                    x
                )
                (
                vertex $ do
                    xId <- x
                    yId <- y
                    send xId yId
                    y
                )





            -- OR get neighbours from graph and expect that many times...


-- node :: ShenoyShaferNode v a b -> [ProcessId] -> Process ()
-- node v (n:neighbours) = do
-- 
--     (valuation, senderId) <- expect
-- 
--     send 



    
-- to spawn a node we need the process id of the node it is going to send a message to in the collect phase.
--


    














