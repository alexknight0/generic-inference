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

data Implementation = Baseline | Local I.Mode deriving Show

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

singleTarget :: Implementation -> [G.Graph Natural Double] -> D.Query Natural -> IO [Double]
singleTarget Baseline graphs q = pure $ H.singleTarget graphs q.sources q.target infinity
singleTarget (Local mode) graphs q = fromRight $ ST.singleTarget mode graphs q.sources q.target


