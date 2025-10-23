{-# LANGUAGE BangPatterns #-}
module Benchmarks.FastFourierTransform.Baseline
    ( dft )
where

import           Control.Monad
import           Control.Monad.ST
import           Data.Bits
import           Data.Complex
import qualified Data.Vector                                     as V
import qualified Data.Vector.Mutable                             as MV
import           LocalComputation.Instances.FastFourierTransform

import           Data.Array.CArray                               (createCArray)
import           Data.Array.CArray.Base                          (CArray)
import           Data.Complex
import           Foreign.Marshal                                 (pokeArray)
import qualified Math.FFT                                        as FFT

import qualified Data.Array.IArray                               as I

-- TODO: can replace with listArray?
createComplexArray :: [FourierComplex] -> IO (CArray Int (Complex Double))
createComplexArray xs = createCArray (0, length xs' - 1) (\ptr -> pokeArray ptr xs')
    where
        xs' = fmap (\(FourierComplex x) -> x) xs

dft :: [FourierComplex] -> IO [Complex Double]
dft samples = fft' samples -- fmap (I.elems . FFT.dft) (createComplexArray samples)

alternativeDFT :: [FourierComplex] -> IO [Complex Double]
alternativeDFT xs = alternativeDFT' $ fmap (\(FourierComplex x) -> x) xs

alternativeDFT' :: [Complex Double] -> IO [Complex Double]
alternativeDFT' [x] = pure [x]
alternativeDFT' a  = pure y
    where
        n = length a
        theta :: Double
        theta = -2 * pi / (fromIntegral n)

        w = map (\i -> cos(theta * (fromIntegral i)) :+ sin(theta * (fromIntegral i))) [0.. n-1]

        -- TODO: Probably can be more efficent by doing both at once.
        yEven = map fst . filter (\(_, x) -> even x) $ zip a [0..]
        yOdd  = map fst . filter (\(_, x) -> odd x)  $ zip a [0..]

        -- TODO: is ++ here super inefficent?
        -- middle = div n 2

        belowMiddle    = zipWith3 (\wElem oddElem evenElem -> evenElem + wElem * oddElem) w yOdd yEven
        middleAndAbove = zipWith3 (\wElem oddElem evenElem -> evenElem - wElem * oddElem) w yOdd yEven

        y = belowMiddle ++ middleAndAbove



foobar :: ()
foobar = undefined
{-

>>> alternativeDFT' [(1 :+ 0), (2 :+ 0), (3 :+ 0), (4 :+ 0)]
[3.0 :+ 0.0,3.0000000000000004 :+ (-4.0),(-1.0) :+ 0.0,2.9999999999999996 :+ 4.0]

-}

--------------------------------------------------------------------------------
-- Beow
--------------------------------------------------------------------------------

fft' :: [FourierComplex] -> IO [Complex Double]
fft' xs = pure $ V.toList $ fft $ V.fromList $ fmap (\(FourierComplex x) -> x) xs

-- | Compute forward FFT (un-normalized). Length must be a power of two.
fft :: V.Vector (Complex Double) -> V.Vector (Complex Double)
fft vec
  | n == 0 = V.empty
  | popCount n /= 1 = error "fft: length must be power of two"
  | otherwise = runST $ do
      mvec <- V.thaw vec
      bitReversePermuteInplace mvec
      -- iterative Cooley-Tukey
      let go len
            | len < n = do
                let half = len
                    step = len * 2
                    -- wn = e^{-2πi / step}
                    ang = -2 * pi / fromIntegral step
                forM_ [0, step .. n-1] $ \start ->
                  forM_ [0 .. half - 1] $ \k -> do
                    let twiddle = cis (ang * fromIntegral k)
                        i = start + k
                        j = i + half
                    xi <- MV.read mvec i
                    xj <- MV.read mvec j
                    let t = twiddle * xj
                    MV.write mvec i (xi + t)
                    MV.write mvec j (xi - t)
                go (step)
            | otherwise = return ()
      go 1
      V.freeze mvec
  where
    n = V.length vec

-- | Inverse FFT, normalized (returns original samples).
ifft :: V.Vector (Complex Double) -> V.Vector (Complex Double)
ifft vec
  | V.null vec = V.empty
  | otherwise =
      let n = V.length vec
          conjIn = V.map conjugate vec
          trans = fft conjIn
          conjOut = V.map conjugate trans
          scale = 1 / fromIntegral n
      in V.map (fmap (* scale)) conjOut

-- | Bit-reversal permutation in-place on a mutable vector.
bitReversePermuteInplace :: MV.MVector s (Complex Double) -> ST s ()
bitReversePermuteInplace mvec = do
  let n = MV.length mvec
      l = countTrailingZeros n  -- since n power of two, this is log2 n
  forM_ [0 .. n-1] $ \i -> do
    let j = bitReverse i l
    when (i < j) $ do
      xi <- MV.read mvec i
      xj <- MV.read mvec j
      MV.write mvec i xj
      MV.write mvec j xi

-- | bitReverse i with width bits (i.e., reverse the low `bits` bits of i).
bitReverse :: Int -> Int -> Int
bitReverse x bits = go x bits 0
  where
    go !_ 0 !acc = acc
    go v k !acc  = go (v `shiftR` 1) (k - 1) ((acc `shiftL` 1) .|. (v .&. 1))









