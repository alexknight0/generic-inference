module Benchmark.ShortestPath.SingleTarget
    (
        benchmarks
    )
where

-- TODO: don't want a dependency of benchmark on test - move queries to a place inside benchmark.
import qualified Benchmark.Data.ShortestPath                          as D

import qualified Benchmark.Baseline.DjikstraSimple                    as H
import           Benchmark.Data.ShortestPath                          (genConnectedQueries,
                                                                       sample)
import           Criterion.Main
import qualified LocalComputation.Graph                               as G
import qualified LocalComputation.Inference                           as I
import qualified LocalComputation.Instances.ShortestPath.SingleTarget as ST
import           LocalComputation.Utils                               (fromRight,
                                                                       infinity)
import           Numeric.Natural                                      (Natural)

import           Control.DeepSeq                                      (NFData,
                                                                       deepseq,
                                                                       rnf)
import           Control.Monad                                        (forM)
import qualified Data.Map.Lazy                                        as M

data Implementation = Baseline | Local I.Mode deriving Show

--------------------------------------------------------------------------------
-- Benchmarks
--------------------------------------------------------------------------------

foobar :: ()
foobar = undefined
{-

>>> p3Small <- D.p3SmallGraph'
>>> sample $ genConnectedQueries 10 p3Small
[Query {sources = [9,10], target = 10},Query {sources = [16,17], target = 17},Query {sources = [25,26], target = 26},Query {sources = [54,55], target = 54},Query {sources = [64,65], target = 64},Query {sources = [27,28], target = 28},Query {sources = [20,21], target = 21},Query {sources = [60,61,68,69], target = 60},Query {sources = [18,19], target = 18},Query {sources = [43,44], target = 43}]

-}

benchmarks :: IO Benchmark
benchmarks = do
    p3Small <- D.p3SmallGraph'
    -- p3Medium <- D.p3MediumGraph'
    -- tmp <- sample $ genConnectedQueries 10 p3Medium
    queries <- sample $ genConnectedQueries 10 p3Small

    shenoy      <- singleTarget (Local I.Shenoy)     [p3Small] queries
    bruteForce  <- singleTarget (Local I.BruteForce) [p3Small] queries
    fusion      <- singleTarget (Local I.Fusion)     [p3Small] queries
    baseline    <- singleTarget (Baseline)            [p3Small] queries

    pure $ bgroup "Shortest Path" [

                  --bench "inparlell-shenoy"     $ nfIO $ fromRight $ ST.singleTarget (I.Shenoy) [p3Medium] (head tmp).sources (head tmp).target
                  bench "localcomputation-shenoy"     $ nfIO shenoy
                , bench "localcomputation-bruteForce" $ nfIO bruteForce
                , bench "localcomputation-fusion"     $ nfIO fusion
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
singleTarget (Local mode) gs qs = pure $ mapM (\q -> fromRight $ ST.singleTarget mode gs q.sources q.target) qs

-- singleTargetWithRandomQuery :: Implementation -> G.Graph Natural Double -> [(Natural, [Natural])] -> IO [Double]
-- singleTargetWithRandomQuery mode graph reverseAdjacencyList = do
--     query <- D.sample $ D.genConnectedQuery reverseAdjacencyList
--     singleTarget mode [graph] query


-- singleTarget :: Implementation -> [G.Graph Natural Double] -> ST.Query Natural -> IO [Double]
-- singleTarget Baseline graphs q = pure $ H.singleTarget graphs q.sources q.target infinity
-- singleTarget (Local mode) graphs q = fromRight $ ST.singleTarget mode graphs q.sources q.target

