{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import qualified Algebra.Graph as Directed
import Algebra.Graph.Undirected
import Data.List (intersperse)
import Data.Binary (Binary)
import GHC.Generics

-- LocalComputation library files
import ValuationAlgebra
import Collect
import Bayesian
import JoinTree
import ShenoyShafer


---- We will need these someday (probably)
import Control.Concurrent (threadDelay)
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Network.Transport.TCP
-- import Data.Tree

----------
-- utils
------------

-- Consider creating a typeclass for variables that has them implement 'hashCode' - nenok uses
-- this to implement the binary heap ordering?

-- -- uses not-undirected graph so commented out for now
showAdjacents :: (Ord a, Show a) => (Directed.Graph a) -> String
showAdjacents graph = concat $ intersperse "\n\n\n" $ fmap (show) (Directed.adjacencyList graph)

data P1Var = F | B | L | D | H deriving (Eq, Ord, Show, Generic, Binary)

data P1Value = P1False | P1True deriving (Enum, Bounded, Show, Eq, Generic, Binary)

p1Valuations :: [BayesValuation P1Var P1Value]
p1Valuations =
    [ getRows $ Columns F [] [0.85, 0.15],
        getRows $ Columns B [] [0.99, 0.01],
        getRows $ Columns L [F] [0.95, 0.4, 0.05, 0.6],
        getRows $ Columns D [F, B] [0.7, 0.03, 0.1, 0.01, 0.3, 0.97, 0.9, 0.99],
        getRows $ Columns H [D] [0.99, 0.3, 0.01, 0.7]
    ]

p1Query :: [Domain P1Var]
p1Query = [[F]]

data MainParameters = MainParameters {
    printJoinTree :: Bool,
    performInference :: Bool
}

main :: IO ()
main = runProcess' $ mainProcess (MainParameters {printJoinTree = True, performInference = True})

mainProcess :: MainParameters -> Process ()
mainProcess params = do

    if printJoinTree params then liftIO $ putStrLn $ showAdjacents $ p1BasicTree else return ()

    if performInference params then p1ShenoyInference else return ()

    -- Wait for a second to make sure all message passing had a chance to finish.
    -- (Should really be 'waiting' on the tasks...)
    liftIO $ threadDelay 1000000

p1BasicTree :: Directed.Graph (CollectNode BayesValuation P1Var P1Value)
p1BasicTree = baseJoinTree p1Valuations p1Query

p1ShenoyInference :: Process ()
p1ShenoyInference = initializeNodes (shenoyJoinTree p1Valuations p1Query)

runProcess' :: Process () -> IO ()
runProcess' process = do

    Right transport <- createTransport (defaultTCPAddr "127.0.0.1" "8080") defaultTCPParameters
    node <- newLocalNode transport initRemoteTable
    runProcess node process
