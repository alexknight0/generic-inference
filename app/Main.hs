{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE OverloadedLists #-}

module Main (main) where

import qualified Algebra.Graph as Directed
import Algebra.Graph.Undirected
import Data.List (intersperse)
import Data.Binary (Binary)
import GHC.Generics
import Data.Char (ord)
import Data.Time (getCurrentTime)
import Control.Monad (when)
import qualified Data.Map as M

-- LocalComputation library files
import ValuationAlgebra
import Collect
import Bayesian
import JoinTree
import ShenoyShafer
import Utils


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
showAdjacents graph = concat $ intersperse "\n" $ fmap (show) (Directed.adjacencyList graph)

showNodes :: (Ord a, Show a) => (Directed.Graph a) -> String
showNodes graph = concat $ intersperse "\n" $ fmap show (Directed.vertexList graph)

data P1Var = VisitToAsia | HasTuberculosis | Smoker | HasLungCancer
           | HasBronchitis | TuberculosisOrCancer | XRayResult | Dyspnea
           deriving (Eq, Ord, Show, Generic, Binary)

data P1Value = P1False | P1True deriving (Enum, Bounded, Show, Eq, Generic, Binary, Ord)

-- These match the ones used in NENOK and in https://online.bayesserver.com/
-- (Only difference from project proposal is smoker).
-- Note the probably values from https://online.bayesserver.com/ round to 1dp after %.
p1Valuations :: [BayesValuation P1Var P1Value]
p1Valuations =
    [ getRows $ Columns [VisitToAsia] [0.99, 0.01],
        getRows $ Columns [HasTuberculosis, VisitToAsia] [0.99, 0.95, 0.01, 0.05],
        getRows $ Columns [Smoker] [0.5, 0.5],
        getRows $ Columns [HasLungCancer, Smoker] [0.99, 0.9, 0.01, 0.1],
        getRows $ Columns [HasBronchitis, Smoker] [0.7, 0.4, 0.3, 0.6],
        getRows $ Columns [TuberculosisOrCancer, HasTuberculosis, HasLungCancer] [1, 0, 0, 0, 0, 1, 1, 1],
        getRows $ Columns [XRayResult, TuberculosisOrCancer] [0.95, 0.02, 0.05, 0.98],
        getRows $ Columns [Dyspnea, TuberculosisOrCancer, HasBronchitis] [0.9, 0.2, 0.3, 0.1, 0.1, 0.8, 0.7, 0.9]
    ]

p1ProbabilityQueries :: [ProbabilityQuery P1Var P1Value]
p1ProbabilityQueries = [
          (M.singleton HasTuberculosis P1True, M.singleton VisitToAsia P1True)
        , (M.singleton TuberculosisOrCancer P1True, fromListAssertDisjoint [(VisitToAsia, P1True), (Smoker, P1False), (HasBronchitis, P1True)])
        , (M.singleton TuberculosisOrCancer P1True, fromListAssertDisjoint [(VisitToAsia, P1True), (HasTuberculosis, P1True), (Smoker, P1False), (HasBronchitis, P1True)])
        , (M.singleton TuberculosisOrCancer P1True, fromListAssertDisjoint [(VisitToAsia, P1True), (HasTuberculosis, P1False), (Smoker, P1False), (HasBronchitis, P1True)])
    ]

p1Queries :: [Domain P1Var]
p1Queries = [
          [VisitToAsia]
        , [HasTuberculosis]
        , [Smoker]
        , [HasLungCancer]
        , [HasBronchitis]
        , [TuberculosisOrCancer]
        , [XRayResult]
        , [Dyspnea]
    ]

data P2Var = Name | Noun | Colour | ID | DeviceID | DeviceType deriving (Eq, Ord, Show, Generic, Binary)

type P2Value = P1Value

-- These are nonsense valuations just used for demoing the join tree construction process
-- using the only half-implemented valuation algebra instance we have.
p2Valuations :: [BayesValuation P2Var P2Value]
p2Valuations =
    [ getRows $ Columns [Name, ID] [1, 1],
        --getRows $ Columns Noun [Colour] [1, 1],
        getRows $ Columns [ID, DeviceID] [1, 1],
        getRows $ Columns [DeviceID, DeviceType] [1, 1]
    ]

p2Query :: [Domain P2Var]
p2Query = [[Name, DeviceType]]

data MainParameters = MainParameters {
    printP1JoinTree :: Bool,
    printP2JoinTree :: Bool,
    performP1ShenoyInference :: Bool,
    queryP1Network :: Bool,
    performP2SeminarInference :: Bool,
    test :: Bool
}

main :: IO ()
main = runProcess' $ mainProcess (MainParameters {
    printP1JoinTree = False,
    printP2JoinTree = False,
    performP1ShenoyInference = False,
    queryP1Network = True,
    performP2SeminarInference = False,
    test = False
})

mainProcess :: MainParameters -> Process ()
mainProcess params = do

    liftIO $ putStrLn "\n\n\n\n\nRunning Program...\n\n"

    when (printP1JoinTree params) $
        liftIO $ print $ length $ showAdjacents $ p1BasicTree

    when (printP2JoinTree params) $
        liftIO $ putStrLn $ showNodes $ p2BasicTree

    when (performP1ShenoyInference params) $ do
        results <- answerQueriesM p1Valuations p1Queries
        liftIO $ print (zip p1Queries (map normalize results))

    when (queryP1Network params) $ do
        results <- queryNetwork p1ProbabilityQueries p1Valuations
        liftIO $ print (zip p1ProbabilityQueries results)

    when (performP2SeminarInference params) $
        p2SeminarInference

    when (test params) $ do
        liftIO $ putStrLn p1Test

p1BasicTree :: Directed.Graph (CollectNode BayesValuation P1Var P1Value)
p1BasicTree = baseJoinTree p1Valuations p1Queries

p1Test :: String
p1Test = showAsRows $ normalize $ project (combines xs) [HasTuberculosis]
    where
        xs :: [BayesValuation P1Var P1Value]
        xs = p1Valuations
        -- xs = [
        --       getRows $ Columns [VisitToAsia] [0.99, 0.01]
        --     , getRows $ Columns [HasTuberculosis, VisitToAsia] [0.99, 0.95, 0.01, 0.05]
        --     ]


p2BasicTree :: Directed.Graph (CollectNode BayesValuation P2Var P2Value)
p2BasicTree = baseJoinTree p2Valuations p2Query

p2SeminarTree :: Graph (CollectNode BayesValuation P2Var P2Value)
p2SeminarTree = overlays [ connect a x, connect b x, connect x z, connect z q, connect z c]
    where
        a = vertex $ create (fromIntegral $ ord 'A') [Name, ID] (identity)
        b = vertex $ create (fromIntegral $ ord 'B') [ID, DeviceID] (identity)
        c = vertex $ create (fromIntegral $ ord 'C') [DeviceID, DeviceType] (identity)
        q = vertex $ create (fromIntegral $ ord 'Q') [Name, DeviceType] (identity)
        x = vertex $ create (fromIntegral $ ord 'X') [Name, ID, DeviceID] (identity)
        z = vertex $ create (fromIntegral $ ord 'Z') [Name, DeviceID, DeviceType] (getRows ColumnsIdentity)

p2SeminarInference :: Process ()
p2SeminarInference = initializeNodes p2SeminarTree >> pure ()

p1ShenoyInference :: Process ()
p1ShenoyInference = initializeNodes (shenoyJoinTree p1Valuations p1Queries) >> pure ()

runProcess' :: Process () -> IO ()
runProcess' process = do

    Right transport <- createTransport (defaultTCPAddr "127.0.0.1" "8080") defaultTCPParameters
    node <- newLocalNode transport initRemoteTable
    runProcess node process
