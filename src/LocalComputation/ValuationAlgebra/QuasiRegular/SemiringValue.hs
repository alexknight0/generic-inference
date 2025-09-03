{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LocalComputation.ValuationAlgebra.QuasiRegular.SemiringValue
    ( QSemiringValue (quasiInverse)
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
class (SemiringValue a) => QSemiringValue a where
    quasiInverse :: a -> a

    -- TODO:
    -- zero and one also has to satisfy some special laws with
    -- respect to idempotency and monotonicity to
    -- work for path problems? pdf page 257 gen. inf.

-- | A value from the tropical semiring of all real numbers. Detailed page 232 of "Generic Inference" (Pouly & Kohlas, 2012).
newtype TropicalSemiringValue = T Double deriving (Ord, Num, Real, Enum, Show, NFData, Eq, Generic, Read, Binary)

toDouble :: TropicalSemiringValue -> Double
toDouble (T x) = x

instance SemiringValue TropicalSemiringValue where
    add = min
    multiply = (+)
    zero = T (read "Infinity" :: Double)
    one = 0

instance QSemiringValue TropicalSemiringValue where
    quasiInverse x
        | x >= 0 = 0
        | otherwise = T (read "-Infinity" :: Double)
