module Benchmarks.ShortestPath.SingleTarget
    (
        benchmarks
    )
where

-- TODO: !!!!!!!!!!!!!!!! BEFORE BENCHMARKING !!!!!!!!!!!!!!!!!!!!!
-- TODO: !!!!!!!!!!!!!!!! BEFORE BENCHMARKING !!!!!!!!!!!!!!!!!!!!!
-- TODO: !!!!!!!!!!!!!!!! BEFORE BENCHMARKING !!!!!!!!!!!!!!!!!!!!!
-- 1. Make fusion construct a better join tree for itself.
-- 2. Make quasiregular split the graph nicely for itself.

import qualified Benchmarks.ShortestPath.SingleTarget.Data            as D

import qualified Benchmarks.ShortestPath.SingleTarget.Baseline        as H
import           Benchmarks.ShortestPath.SingleTarget.Data            (genConnectedQueries,
                                                                       sample)
import           Criterion.Main
import qualified LocalComputation.Graph                               as G
import qualified LocalComputation.Inference                           as I
import qualified LocalComputation.Instances.ShortestPath.SingleTarget as ST
import           LocalComputation.Utils                               (fromRight,
                                                                       infinity)
import           Numeric.Natural                                      (Natural)

import           Control.DeepSeq                                      (deepseq)
import qualified LocalComputation.Inference.JoinTree.Diagram          as D

data Implementation = Baseline | Local I.Mode deriving Show

--------------------------------------------------------------------------------
-- Benchmarks
--------------------------------------------------------------------------------

benchmarks :: IO [Benchmark]
benchmarks = do
    -- p3Small <- D.p3SmallGraph'
    graph <- sample $ D.genGraph 50 200
    -- p3Medium <- D.p3MediumGraph'
    -- tmp <- sample $ genConnectedQueries 10 p3Medium
    queries <- sample $ genConnectedQueries 10 graph

    bruteForce  <- singleTarget (Local I.BruteForce) [graph] queries
    fusion      <- singleTarget (Local I.Fusion)     [graph] queries
    shenoy      <- singleTarget (Local I.Shenoy)     [graph] queries
    baseline    <- singleTarget (Baseline)           [graph] queries

    pure $ pure $ bgroup "Shortest Path" [

                  --bench "inparlell-shenoy"     $ nfIO $ fromRight $ ST.singleTarget (I.Shenoy) [p3Medium] (head tmp).sources (head tmp).target
                  bench "localcomputation-bruteForce" $ nfIO bruteForce
                , bench "localcomputation-fusion"     $ nfIO fusion
                , bench "localcomputation-shenoy"     $ nfIO shenoy
                , bench "djikstra"                    $ nfIO baseline
            ]

-- | A uniform interface for querying the single target shortest path solution using any implementation.
-- Returns an IO action that returns another IO action. When the initial IO action is bound it
-- performs the setup for problem and then returns an IO action that performs the computation that
-- actually needs to be benchmarked.
singleTarget :: Implementation -> [G.Graph Natural Double] -> [ST.Query Natural] -> IO (IO [[Double]])
singleTarget Baseline gs qs = do
    -- We peform arc reversal during setup as we want to compare the baseline and
    -- localcomputation algorithms as if both were single source or single target.
    let reversedGraphs = map G.flipArcDirections gs

    reversedGraphs `deepseq` pure $ do
        -- We do count merging as a cost of using the package.
        let g = H.merges H.empty (map H.fromGraph reversedGraphs)

        -- Compute solutions
        pure $ map (\q -> H.singleSource g q.target q.sources infinity) qs
singleTarget (Local mode) gs qs = pure $ mapM (\q -> fromRight $ ST.singleTarget D.def mode gs q) qs

-- singleTargetWithRandomQuery :: Implementation -> G.Graph Natural Double -> [(Natural, [Natural])] -> IO [Double]
-- singleTargetWithRandomQuery mode graph reverseAdjacencyList = do
--     query <- D.sample $ D.genConnectedQuery reverseAdjacencyList
--     singleTarget mode [graph] query


-- singleTarget :: Implementation -> [G.Graph Natural Double] -> ST.Query Natural -> IO [Double]
-- singleTarget Baseline graphs q = pure $ H.singleTarget graphs q.sources q.target infinity
-- singleTarget (Local mode) graphs q = fromRight $ ST.singleTarget mode graphs q.sources q.target

