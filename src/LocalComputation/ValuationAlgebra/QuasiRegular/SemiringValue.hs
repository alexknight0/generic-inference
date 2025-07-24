{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LocalComputation.ValuationAlgebra.QuasiRegular.SemiringValue
    ( QuasiRegularSemiringValue (quasiInverse)
    , SemiringValue (add, multiply, zero, one)
    , TropicalSemiringValue (T)
    , toDouble
    )
where

import           LocalComputation.ValuationAlgebra.Semiring

-- Typeclasses
import           Control.DeepSeq                            (NFData)
import           Data.Binary                                (Binary)
import           GHC.Generics                               (Generic)

-- | A Quasi-regular semiring is also known as a 'closed' semiring
class (SemiringValue a) => QuasiRegularSemiringValue a where
    quasiInverse :: a -> a

    -- zero and one also has to satisfy some special laws with
    -- respect to idempotency and monotonicity to
    -- work for path problems? pdf page 257 gen. inf.

-- | A value from the tropical semiring of all real numbers. Detailed page 232 of "Generic Inference" (Pouly & Kohlas, 2012).
newtype TropicalSemiringValue = T Double deriving (Ord, Num, Show, NFData, Eq, Generic, Read, Binary)

toDouble :: TropicalSemiringValue -> Double
toDouble (T x) = x

instance SemiringValue TropicalSemiringValue where
    add = min
    multiply = (+)
    zero = T (read "Infinity" :: Double)
    one = 0

instance QuasiRegularSemiringValue TropicalSemiringValue where
    quasiInverse x
        | x >= 0 = 0
        | otherwise = T (read "-Infinity" :: Double)

-- instance Binary (TropicalSemiringValue) where
--     put (T x) = put x
--
--     get = do
--         x <- get
--         pure (T x)
