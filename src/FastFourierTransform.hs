{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

module FastFourierTransform
    ( query
    , FourierComplex (FourierComplex)
    , createComplexArray
    , createComplexArray'
    )
where

import           SemiringValuationAlgebra
import           ShenoyShafer
import           Utils
import           ValuationAlgebra

import           Data.Complex                (Complex ((:+)))
import qualified Data.Complex                as C
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import           Math.FFT                    (dftN)
import           Numeric.Natural
import Debug.Trace

import           Control.Distributed.Process (Process, liftIO)

-- Typeclasses
import           Control.DeepSeq             (NFData)
import           Data.Binary                 (Binary)
import           GHC.Generics                (Generic)

import           Foreign.Marshal      (pokeArray)
import           Math.FFT             (dft)
import Data.Array.CArray (createCArray)
import Data.Array.CArray.Base (CArray)
import System.IO (hPutStrLn, stderr)

newtype FourierComplex = FourierComplex (C.Complex Double) deriving newtype (Num, Fractional, Binary, Show, NFData, Eq, Generic)


-- TODO update doc.
{- | Variables used in the FastFourierValuation

(X i) and (Y j) are variables indicating the i_th and j_th bits in the binary number representation of x and y respectively.
(F i) is a variable representing the i_th sample of a function representing the sampled signal.
-}
data FastFourierVariable = X Natural | Y Natural deriving (Eq, Ord, Binary, Generic, Show)

type FastFourierValuation = SemiringValuation FourierComplex FastFourierVariable Natural

instance SemiringValue FourierComplex where
    multiply = (*)
    add = (+)

{- | For a given N, returns the smallest m such that `N <= 2^m` -}
getM :: Natural -> Natural
getM n = ceiling $ logBase 2 (fromIntegral n :: Double)

getE' :: Natural -> Natural -> Natural -> Natural -> Natural -> Complex Double
getE' m j l nj kl = exp $ negate $ (/) (2 * pi * i * nj' * kl') (2 ^ (m - j - l))
    where
        i = (0 :+ 1)
        nj' = (fromIntegral (nj) :+ 0)
        kl' = (fromIntegral (kl) :+ 0)

-- Variables X_j and Y_l are binary numbers, so only take values 0 or 1.
getE :: Natural -> Natural -> Natural -> FastFourierValuation
getE m j l = Table $ map row [(0, 0), (0, 1), (1, 0), (1, 1)]
    where
        row :: (Natural, Natural) -> Row FourierComplex FastFourierVariable Natural
        row (x, y) = Row (fromListAssertDisjoint [(X j, x), (Y l, y)]) (FourierComplex $ getE' m j l x y)

getKnowledgebase :: [FourierComplex] -> [FastFourierValuation]
getKnowledgebase samples = f : [getE m j l | j <- [0 .. m-1], l <- [0 .. m-1-j]]
    where
        m = getM (fromIntegral $ length samples)

        f :: FastFourierValuation
        f = Table $ zipWith (\x s -> Row (toBinaryVariableSet m x X) s) [0..] samples

-- TODO not sure how it handles a non 2-power number of samples.
query :: [FourierComplex] -> [Natural] -> Process [FourierComplex]
query samples qs = do

    result <- answerQueryM (getKnowledgebase samples) queryDomain
    pure $ map (findBinaryValue result m) qs

    where
        m = getM (fromIntegral $ length samples)

        -- Each query has the same domain - the domain of all bits of the Y, i.e. Y_0 to Y_m-1
        queryDomain :: Domain FastFourierVariable
        queryDomain = S.fromList $ map Y [0 .. m-1]

findBinaryValue :: SemiringValuation FourierComplex FastFourierVariable Natural -> Natural -> Natural -> FourierComplex
findBinaryValue table numDigits x = findValue (toBinaryVariableSet numDigits x Y) table

toBinaryVariableSet :: Natural -> Natural -> (Natural -> FastFourierVariable) -> M.Map FastFourierVariable Natural
toBinaryVariableSet numDigits x f = fromListAssertDisjoint $ zipWith (\i y -> (f i, fromIntegral $ fromEnum y)) [0..] (reverse $ toBinaryLeadingZeroes numDigits x)

-- todo can replace with listArray?
createComplexArray :: [FourierComplex] -> IO (CArray Int (Complex Double))
createComplexArray xs = createCArray (0, length xs' - 1) (\ptr -> pokeArray ptr xs')
    where
        xs' = fmap (\(FourierComplex x) -> x) xs

createComplexArray' :: [Complex Double] -> IO (CArray Int (Complex Double))
createComplexArray' xs = createCArray (0, length xs - 1) (\ptr -> pokeArray ptr xs)

