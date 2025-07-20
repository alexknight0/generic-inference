module Benchmark (
    allBenchmarks
) where

import qualified Benchmark.ShortestPath.SingleTarget as ST

allBenchmarks :: IO ()
allBenchmarks = ST.benchmarks


