module Benchmark (
    allBenchmarks
) where

import qualified Benchmark.BayesianNetwork           as BN
import qualified Benchmark.FastFourierTransform      as FFT
import qualified Benchmark.ShortestPath.SingleTarget as ST

import           Criterion.Main

allBenchmarks :: IO ()
allBenchmarks = do
    benchmarks <- sequence [
                          --  ST.benchmarks
                          --  FFT.benchmarks
                            BN.benchmarks
                          ]
    defaultMain benchmarks

-- --    defaultMainWith customConfig [
-- customConfig :: Config
-- customConfig = defaultConfig
--   { resamples = 10000       -- Number of bootstrap resamples (default is 1000)
--   , timeLimit = 100
--   }
--




