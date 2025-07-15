module Benchmark.Baseline.FFT
    ( dft )
where

import           LocalComputation.Instances.FastFourierTransform

import           Data.Array.CArray                               (createCArray)
import           Data.Array.CArray.Base                          (CArray)
import           Data.Complex                                    (Complex)
import           Foreign.Marshal                                 (pokeArray)
import qualified Math.FFT                                        as FFT

-- TODO: can replace with listArray?
createComplexArray :: [FourierComplex] -> IO (CArray Int (Complex Double))
createComplexArray xs = createCArray (0, length xs' - 1) (\ptr -> pokeArray ptr xs')
    where
        xs' = fmap (\(FourierComplex x) -> x) xs

dft :: [FourierComplex] -> IO (CArray Int (Complex Double))
dft samples = fmap FFT.dft (createComplexArray samples)
