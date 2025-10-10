module Benchmarks (
    allBenchmarks
) where

import qualified Benchmarks.BayesianNetwork           as BN
import qualified Benchmarks.FastFourierTransform      as FFT
import qualified Benchmarks.ShortestPath.SingleTarget as ST

import           Criterion.Main
import           Criterion.Types                      (Config, resamples)

import           Data.Char                            (toLower)
import           System.Environment                   (getArgs)

allBenchmarks :: IO ()
allBenchmarks = do
    args <- getArgs
    let lowercaseArgs = map (map toLower) args

    if "ops" `elem` lowercaseArgs then
        -- Counting the number of operations
        ST.countOperations
    else do
        -- Performance testing
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





