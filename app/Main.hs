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
import Data.Char (ord)
import Data.Time (getCurrentTime)

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

data P2Var = Name | Age | Birthplace | ID | DeviceID | DeviceType deriving (Eq, Ord, Show, Generic, Binary)

type P2Value = P1Value

p2Valuations :: [BayesValuation P2Var P2Value]
p2Valuations =
    [ getRows $ Columns Name [Age, Birthplace] [1, 1],
        getRows $ Columns ID [Name] [1, 1],
        getRows $ Columns ID [DeviceID] [1, 1],
        getRows $ Columns DeviceID [DeviceType] [1, 1]
    ]

p2Query :: [Domain P2Var]
p2Query = [[Name, DeviceType]]

data MainParameters = MainParameters {
    printP1JoinTree :: Bool,
    printP2JoinTree :: Bool,
    performInference :: Bool,
    performP2SeminarInference :: Bool
}

main :: IO ()
main = runProcess' $ mainProcess (MainParameters {
    printP1JoinTree = False,
    printP2JoinTree = False,
    performInference = False,
    performP2SeminarInference = True
})

mainProcess :: MainParameters -> Process ()
mainProcess params = do

    if printP1JoinTree params then liftIO $ putStrLn $ showAdjacents $ p1BasicTree else return ()

    if printP2JoinTree params then liftIO $ putStrLn $ showAdjacents $ p2BasicTree else return ()

    if performInference params then p1ShenoyInference else return ()

    if performP2SeminarInference params then p2SeminarInference else return ()

    -- Wait for a second to make sure all message passing had a chance to finish.
    -- (Should really be 'waiting' on the tasks...)
    liftIO $ putStrLn "Waiting for threads to finish..."
    liftIO $ threadDelay 100000000

p1BasicTree :: Directed.Graph (CollectNode BayesValuation P1Var P1Value)
p1BasicTree = baseJoinTree p1Valuations p1Query

p2BasicTree :: Directed.Graph (CollectNode BayesValuation P2Var P2Value)
p2BasicTree = baseJoinTree p2Valuations p2Query

p2SeminarTree :: Graph (CollectNode BayesValuation P2Var P2Value)
p2SeminarTree = overlays [ connect a b, connect b z, connect q z, connect z x, connect x c, connect x d ]

    where
        a = vertex $ create (fromIntegral $ ord 'A')  [Name, Age, Birthplace] (getRows ColumnsNull)
        b = vertex $ create (fromIntegral $ ord 'B') [ID, Name] (getRows ColumnsNull)
        c = vertex $ create (fromIntegral $ ord 'C') [ID, DeviceID] (getRows ColumnsNull)
        d = vertex $ create (fromIntegral $ ord 'D') [DeviceID, DeviceType] (getRows ColumnsNull)
        q = vertex $ create (fromIntegral $ ord 'Q') [Name, DeviceType] (getRows ColumnsNull)
        z = vertex $ create (fromIntegral $ ord 'Z') [Name, ID, DeviceType] (getRows ColumnsIdentity)
        x = vertex $ create (fromIntegral $ ord 'X') [ID, DeviceID, DeviceType] (getRows ColumnsIdentity)

p2SeminarInference :: Process ()
p2SeminarInference = initializeNodes p2SeminarTree

p1ShenoyInference :: Process ()
p1ShenoyInference = initializeNodes (shenoyJoinTree p1Valuations p1Query)

runProcess' :: Process () -> IO ()
runProcess' process = do

    Right transport <- createTransport (defaultTCPAddr "127.0.0.1" "8080") defaultTCPParameters
    node <- newLocalNode transport initRemoteTable
    runProcess node process
