module Benchmarks.FastFourierTransform
    (
        benchmarks
    )
where

-- TODO: !!!!!!!!!!!!!!!! BEFORE BENCHMARKING !!!!!!!!!!!!!!!!!!!!!
-- TODO: !!!!!!!!!!!!!!!! BEFORE BENCHMARKING !!!!!!!!!!!!!!!!!!!!!
-- TODO: !!!!!!!!!!!!!!!! BEFORE BENCHMARKING !!!!!!!!!!!!!!!!!!!!!
-- 1. Make fusion construct a better join tree for itself.
-- 2. Make quasiregular split the graph nicely for itself.
-- And after:
-- 1. Test singleTarget with a multiple query architecture, splitting
--    the 'domain' into pairs of (target, source) instead of a single
--    large query.

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

