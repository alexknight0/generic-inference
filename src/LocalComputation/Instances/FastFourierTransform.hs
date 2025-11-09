{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

module LocalComputation.Instances.FastFourierTransform
    ( query
    , unsafeQuery
    , FourierComplex (FourierComplex)
    , toBinaryVariableSet
    )
where

import           LocalComputation.Utils
import           LocalComputation.ValuationAlgebra.Semiring

import           Data.Complex                               (Complex ((:+)))
import qualified Data.Complex                               as C
import qualified Data.Map                                   as M
import qualified Data.Set                                   as S
import           Numeric.Natural


-- Typeclasses
import           Control.DeepSeq                            (NFData)
import           Data.Binary                                (Binary)
import qualified Data.Hashable                              as H
import           GHC.Generics                               (Generic)

import qualified Control.Monad.IO.Class                     as M
import           Data.Maybe                                 (fromJust)
import qualified LocalComputation.Inference                 as I

newtype FourierComplex = FourierComplex (C.Complex Double) deriving newtype (Num, Fractional, Binary, Show, NFData, Eq, Generic)

{- | Variables used in the FastFourierValuation

(X i) and (Y j) are variables indicating the i_th and j_th bits in the binary number representation of x and y respectively.
-}
data FastFourierVariable = X Natural | Y Natural deriving (Eq, Ord, Binary, Generic, Show, H.Hashable, NFData)

type FastFourierValuation = Valuation FourierComplex Natural FastFourierVariable

instance SemiringValue FourierComplex where
    add = (+)
    multiply = (*)

    -- Not used.
    zero = error "Not implemented"
    one = error "Not implemented"

getE' :: Natural -> Natural -> Natural -> Natural -> Natural -> Complex Double
getE' m j l nj kl = exp $ negate $ (/) (2 * pi * i * nj' * kl') (2 ^ (m - j - l))
    where
        i = (0 :+ 1)
        nj' = (fromIntegral nj :+ 0)
        kl' = (fromIntegral kl :+ 0)

-- Variables X_j and Y_l are binary numbers, so only take values 0 or 1.
getE :: Natural -> Natural -> Natural -> FastFourierValuation
getE m j l = fromPermutationMap rows
    where
        rows = fromListAssertDisjoint (map row [(0, 0), (0, 1), (1, 0), (1, 1)])

        row :: (Natural, Natural) -> (M.Map FastFourierVariable Natural, FourierComplex)
        row (x, y) = ((fromListAssertDisjoint [(X j, x), (Y l, y)]), (FourierComplex $ getE' m j l x y))

getKnowledgebase :: [FourierComplex] -> [FastFourierValuation]
getKnowledgebase samples = f : [getE m j l | j <- [0 .. m-1], l <- [0 .. m-1-j]]
    where
        m = fromJust $ integerLogBase2 (fromIntegral $ length samples)

        f :: FastFourierValuation
        f = fromPermutationMap rows
            where
                rows = fromListAssertDisjoint $ zipWith (\x s -> (toBinaryVariableSet m x X, s)) [0..] samples

{- | Calculates the fourier transform from the given samples and returns the corresponding values for the given y values.

Only operates if the number of samples is > 1 and is a power of two (I think theoretically this
could be expanded to any number, but that hasn't been done here). Returns Nothing if and only if
the number of samples is > 1 and not a power of two.
-}
query :: (M.MonadIO m) => I.Mode -> [FourierComplex] -> [Natural] -> Maybe (m [FourierComplex])
query mode samples qs = case integerLogBase2 (fromIntegral $ length samples) of
    Nothing -> Nothing
    (Just 0) -> Nothing
    (Just m) -> Just $ do
        -- Each query has the same domain - the domain of all bits of the Y, i.e. Y_0 to Y_m-1
        let queryDomain = S.fromList $ map Y $ [0 .. m-1]

        -- let result = fromRight $ fusion (getKnowledgebase samples) queryDomain
        result <- I.unsafeQuery mode (getKnowledgebase samples) queryDomain
        pure $ map (findBinaryValue result m) qs

-- | An unsafe version of `query` - throws when `query` would return `Nothing`.
unsafeQuery :: (M.MonadIO m) => I.Mode -> [FourierComplex] -> [Natural] -> m [FourierComplex]
unsafeQuery mode samples qs = case query mode samples qs of
                                Just result -> result
                                Nothing     -> error "Invalid call to `query`"


findBinaryValue :: FastFourierValuation -> Natural -> Natural -> FourierComplex
findBinaryValue table numDigits x = findValue (toBinaryVariableSet numDigits x Y) table

{- | Returns the given number in binary.

>>  toBinaryVariableSet 3 6 id
fromList [(0,0),(1,1),(2,1)]
                       ^ ^
                       | |___ 1 is present...
                       |___ ... in binary position 2


>>> toBinaryVariableSet 3 6 X
fromList [(X 0,0),(X 1,1),(X 2,1)]
-}
toBinaryVariableSet :: (Ord a) => Natural -> Natural -> (Natural -> a) -> M.Map a Natural
toBinaryVariableSet numDigits x f = fromListAssertDisjoint $ zipWith (\i y -> (f i, fromIntegral $ fromEnum y)) [0..] (reverse $ toBinaryLeadingZeroes numDigits x)

