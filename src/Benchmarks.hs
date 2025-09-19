module Benchmarks (
    allBenchmarks
) where

import qualified Benchmarks.BayesianNetwork           as BN
import qualified Benchmarks.FastFourierTransform      as FFT
import qualified Benchmarks.ShortestPath.SingleTarget as ST

import           Criterion.Main

allBenchmarks :: IO ()
allBenchmarks = do
    benchmarks <- sequence [
                            ST.benchmarks
                          -- , FFT.benchmarks
                          -- , BN.benchmarks
                          ]
    defaultMain (concat benchmarks)

-- --    defaultMainWith customConfig [
-- customConfig :: Config
-- customConfig = defaultConfig
--   { resamples = 10000       -- Number of bootstrap resamples (default is 1000)
--   , timeLimit = 100
--   }
--




