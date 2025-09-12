module Benchmarks.FastFourierTransform
    (
        benchmarks
    )
where

-- TODO: don't want a dependency of benchmark on test - move queries to a place inside benchmark.

import           Benchmarks.FastFourierTransform.Baseline        (dft)
import           Criterion.Main
import           LocalComputation.Instances.FastFourierTransform (query')
import           LocalComputation.LocalProcess                   (runProcessLocal)

benchmarks :: IO [Benchmark]
benchmarks = do
    pure $ pure $ bgroup "Fast_Fourier_Transform" [
                      bench "localcomputation" $ nfIO $ runProcessLocal $ query' fftInput [0 .. (fromIntegral $ length fftInput - 1)]
                    , bench "FFTW"             $ nfIO $ dft fftInput
                ]

    where
        fftInput = take (2 ^ (5 :: Int)) $ repeat 0

