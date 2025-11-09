module Benchmarks (
    allBenchmarks
) where

import qualified Benchmarks.BayesianNetwork           as BN
import qualified Benchmarks.FastFourierTransform      as FFT
import qualified Benchmarks.ShortestPath.SingleTarget as ST

import           Criterion.Main

import           Data.Char                            (toLower)
import           System.Environment                   (getArgs)

allBenchmarks :: IO ()
allBenchmarks = do
    args <- getArgs
    let lowercaseArgs = map (map toLower) args

    if "ops" `elem` lowercaseArgs then
        -- Counting the number of operations
        sequence_ [
              ST.benchmarkComplexity
            , BN.benchmarkComplexity
        ]

    else do
        -- Performance testing
        benchmarks <- sequence [
                                  ST.benchmarkPerformance
                                , FFT.benchmarks
                                , BN.benchmarkPerformance
                              ]
        defaultMain (concat benchmarks)

