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

data Implementation = Baseline | Local I.Mode deriving Show

--------------------------------------------------------------------------------
-- Benchmarks
--------------------------------------------------------------------------------

benchmarks :: IO Benchmark
benchmarks = do
    p3Small <- D.p3SmallGraph'
    let p3SmallAdjList = G.reverseAdjacencyList p3Small
    -- pPrint p3Small
    -- results <- runProcessLocal $ ST.singleTarget [fmap T p3Small] [68] 69
    -- pPrint results
    queries <- sample $ genConnectedQueries 100 p3Small

    pure $ bgroup "Shortest Path" [
                  bench "localcomputation" $ nfIO $ singleTargetWithRandomQuery (Local I.Shenoy) p3Small p3SmallAdjList
                , bench "djikstra"         $ nfIO $ singleTargetWithRandomQuery (Baseline)       p3Small p3SmallAdjList
            ]

singleTargetWithRandomQuery :: Implementation -> G.Graph Natural Double -> [(Natural, [Natural])] -> IO [Double]
singleTargetWithRandomQuery mode graph reverseAdjacencyList = do
    query <- D.sample $ D.genConnectedQuery reverseAdjacencyList
    singleTarget mode [graph] query

-- | A uniform interface for querying the single target shortest path solution using any implementation.
-- Returns an IO action that returns another IO action. When the initial IO action is bound it
-- performs the setup for problem and then returns an IO action that performs the computation that
-- actually needs to be benchmarked.
singleTarget' :: Implementation -> [G.Graph Natural Double] -> [ST.Query Natural] -> IO (IO [[Double]])
singleTarget' Baseline gs qs = do
    -- We peform arc reversal during setup as we want to compare the baseline and
    -- localcomputation algorithms as if both were single source or single target.
    let reversedGraphs = map (H.create . G.flipArcDirections) gs

    pure $ do
        -- We count merging as a cost of using the package.
        let g = H.merges H.empty reversedGraphs
        pure $ map (\q -> H.singleSource g q.target q.sources infinity) qs

-- singleTarget' (Local mode) gs qs = do
--     fromRight $ ST.singleTarget mode graphs q.sources q.target


-- TODO: Test here. Also remember the ghci caches intermediate results.
testFib :: IO (IO Integer)
testFib = do
    let ans = fib 35
    ans `deepseq` pure $ do
        print ans
        pure ans

fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

foobar :: ()
foobar = undefined
{-

>>> fib 35
9227465

-}

singleTarget :: Implementation -> [G.Graph Natural Double] -> ST.Query Natural -> IO [Double]
singleTarget Baseline graphs q = pure $ H.singleTarget graphs q.sources q.target infinity
singleTarget (Local mode) graphs q = fromRight $ ST.singleTarget mode graphs q.sources q.target

