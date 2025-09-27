module Benchmarks (
    allBenchmarks
) where

import qualified Benchmarks.BayesianNetwork           as BN
import qualified Benchmarks.FastFourierTransform      as FFT
import qualified Benchmarks.ShortestPath.SingleTarget as ST

import           Criterion.Main
import           Criterion.Types                      (Config, resamples)

allBenchmarks :: IO ()
allBenchmarks = do
    benchmarks <- sequence [
                            ST.benchmarks
                          -- , FFT.benchmarks
                            -- , BN.benchmarks
                          ]
    defaultMain (concat benchmarks)

-- defaultMainWith customConfig
-- customConfig :: Config
-- customConfig = defaultConfig {
--     minSamples = 1       -- Number of bootstrap resamples (default is 1000)
-- }





