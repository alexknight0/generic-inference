module Benchmarks (
    allBenchmarks
) where

import qualified Benchmarks.BayesianNetwork            as BN
import qualified Benchmarks.FastFourierTransform       as FFT
import qualified Benchmarks.ShortestPath.SingleTarget  as ST

import           Criterion.Main
import           Criterion.Types                       (Config, resamples)

import           Control.DeepSeq                       (rnf)
import           Control.Exception                     (evaluate)
import           Data.Char                             (toLower)
import qualified Data.List                             as L
import           Data.Maybe                            (fromJust)
import qualified LocalComputation.Inference.Statistics as S
import qualified LocalComputation.Utils                as U
import qualified LocalComputation.ValuationAlgebra     as V
import           System.Environment                    (getArgs, lookupEnv)
import           System.IO                             (IOMode (AppendMode),
                                                        hFlush, hPutStrLn,
                                                        stdout, withFile)

allBenchmarks :: IO ()
allBenchmarks = do
    args <- getArgs
    let lowercaseArgs = map (map toLower) args

    if "ops" `elem` lowercaseArgs then
        -- Counting the number of operations
        sequence_ [
            ST.benchmarkComplexity
            -- BN.benchmarkComplexity
        ]

    else do
        -- Performance testing
        benchmarks <- sequence [
                                -- ST.benchmarkPerformance
                                -- FFT.benchmarks
                                BN.benchmarkPerformance
                              ]
        defaultMain (concat benchmarks)
        -- ST.justDraw










--------------------------------------------------------------------------------
-- Settings
--------------------------------------------------------------------------------

-- defaultMainWith customConfig
-- customConfig :: Config
-- customConfig = defaultConfig {
--     minSamples = 1       -- Number of bootstrap resamples (default is 1000)
-- }




