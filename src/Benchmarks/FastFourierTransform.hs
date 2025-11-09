module Benchmarks.FastFourierTransform
    (
        benchmarks
    )
where

import           Benchmarks.FastFourierTransform.Baseline        (dft)
import           Control.Monad.IO.Class                          (liftIO)
import qualified Control.Monad.IO.Class                          as M
import           Criterion.Main
import           Data.Complex                                    (Complex)
import qualified Data.List                                       as L
import qualified Data.Time.Clock.POSIX                           as C
import qualified GenericInference.Inference                      as I
import qualified GenericInference.Inference.MessagePassing       as MP
import           GenericInference.Problems.FastFourierTransform (FourierComplex,
                                                                  unsafeQuery)

createHeader :: Integer -> Implementation -> Int -> [String]
createHeader timestamp mode size = [show timestamp
                                           , "Fast Fourier Transform"
                                           , show size
                                           , ""
                                           , ""
                                           , ""
                                           , ""
                                           , "No seed"
                                           , modeName mode
                                        ]

solveProblem :: (M.MonadIO m) => Implementation -> Int -> m (Either [Complex Double] [FourierComplex])
solveProblem mode size = case mode of
                            Baseline        -> liftIO $ dft fftInput >>= pure . Left
                            Generic generic -> unsafeQuery generic fftInput [0 .. fromIntegral $ fftInputSize - 1] >>= pure . Right
    where
        fftInputSize = 2 ^ size

        -- No known FFT algorithms have different performance for difference input values.
        fftInput = take fftInputSize $ repeat 0

benchmarks :: IO [Benchmark]
benchmarks = do

    timestamp <- fmap round C.getPOSIXTime :: IO Integer

    pure $ [benchProblem timestamp mode size | size <- [1], mode <- [Baseline, Generic $ I.Fusion]]
    where
        benchProblem timestamp mode size = bench (L.intercalate "/" $ createHeader timestamp mode size) $ nfIO $ solveProblem mode size


data Implementation =  Baseline | Generic I.Mode


modeName :: Implementation -> String
modeName (Baseline)                          = "Baseline"
modeName (Generic I.BruteForce)              = "Brute Force"
modeName (Generic I.Fusion)                  = "Fusion"
modeName (Generic (I.Shenoy MP.Threads))     = "Shenoy (threads)"
modeName (Generic (I.Shenoy MP.Distributed)) = "Shenoy (distributed)"
