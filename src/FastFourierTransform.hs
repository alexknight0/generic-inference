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
    , toBinaryVariableSet
    )
where

import           ShenoyShafer
import           Utils
import           ValuationAlgebra.Semiring

import           Data.Complex                (Complex ((:+)))
import qualified Data.Complex                as C
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import           Numeric.Natural

import           Control.Distributed.Process (Process)

-- Typeclasses
import           Control.DeepSeq             (NFData)
import           Data.Binary                 (Binary)
import           GHC.Generics                (Generic)

import           Data.Array.CArray           (createCArray)
import           Data.Array.CArray.Base      (CArray)
import           Data.Maybe                  (fromJust)
import           Foreign.Marshal             (pokeArray)

newtype FourierComplex = FourierComplex (C.Complex Double) deriving newtype (Num, Fractional, Binary, Show, NFData, Eq, Generic)


-- TODO update doc.
{- | Variables used in the FastFourierValuation

(X i) and (Y j) are variables indicating the i_th and j_th bits in the binary number representation of x and y respectively.
(F i) is a variable representing the i_th sample of a function representing the sampled signal.
-}
data FastFourierVariable = X Natural | Y Natural deriving (Eq, Ord, Binary, Generic, Show)

type FastFourierValuation = SemiringValuation FourierComplex FastFourierVariable Natural

instance SemiringValue FourierComplex where
    add = (+)
    multiply = (*)

    -- Not used. Should probably look into how they would be defined.
    zero = error "Not implemented"
    one = error "Not implemented"

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
        m = fromJust $ integerLogBase2 (fromIntegral $ length samples)

        f :: FastFourierValuation
        f = Table $ zipWith (\x s -> Row (toBinaryVariableSet m x X) s) [0..] samples

{- | Calculates the fourier transform from the given samples and returns the corresponding values for the given y values.

Only operates if the number of samples is > 1 and is a power of two (I think theoretically this could be expanded to any power
of a prime number, but that hasn't been done here). Returns Nothing if and only if the number of samples is > 1 and not a power of two.
-}
query :: [FourierComplex] -> [Natural] -> Process (Maybe [FourierComplex])
query samples qs = case integerLogBase2 (fromIntegral $ length samples) of
    Nothing -> pure Nothing
    (Just 0) -> pure Nothing
    (Just m) -> do
        -- Each query has the same domain - the domain of all bits of the Y, i.e. Y_0 to Y_m-1
        let queryDomain = S.fromList $ map Y $ [0 .. m-1]

        result <- answerQueryM (getKnowledgebase samples) queryDomain
        pure $ pure $ map (findBinaryValue result m) qs

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

