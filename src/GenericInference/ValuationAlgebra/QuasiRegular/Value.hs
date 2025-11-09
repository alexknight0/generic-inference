{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module GenericInference.ValuationAlgebra.QuasiRegular.Value
    ( SemiringValue (quasiInverse)
    , add, multiply, zero, one
    , TropicalSemiringValue (T)
    , toDouble
    )
where

import qualified GenericInference.ValuationAlgebra.Semiring as S

-- Typeclasses
import           Control.DeepSeq                            (NFData)
import           Data.Binary                                (Binary)
import           Data.Vector.Unboxed.Deriving               (derivingUnbox)
import           GHC.Generics                               (Generic)

-- | A Quasi-regular semiring is also known as a 'closed' semiring
class (S.SemiringValue a) => SemiringValue a where
    quasiInverse :: a -> a

-- | A value from the tropical semiring of all real numbers.
--
-- Detailed page 232 of "Generic Inference" (Pouly & Kohlas, 2012).
newtype TropicalSemiringValue = T Double deriving (Ord, Num, Real, Enum, Show, NFData, Eq, Generic, Read, Binary)

-- An instance of Unbox for TropicalSemiringValue.
-- Temporarily used for capatability with LabelledMatrix, when experimenting with unboxed labelled matricies.
-- Can be removed safely.
derivingUnbox "TropicalSemiringValue"
  [t| TropicalSemiringValue -> Double |]
  [| \(T x) -> x |]
  [| T |]

toDouble :: TropicalSemiringValue -> Double
toDouble (T x) = x

instance S.SemiringValue TropicalSemiringValue where
    add = min
    multiply = (+)
    zero = T (read "Infinity" :: Double)
    one = 0

instance SemiringValue TropicalSemiringValue where
    quasiInverse x
        | x >= 0 = 0
        | otherwise = T (read "-Infinity" :: Double)

--------------------------------------------------------------------------------
-- Copy semiring operations for quasi-regular semiring
--------------------------------------------------------------------------------
add :: (SemiringValue a) => a -> a -> a
add = S.add

multiply :: (SemiringValue a) => a -> a -> a
multiply = S.multiply

zero :: (SemiringValue a) => a   -- the additive identity
zero = S.zero

one :: (SemiringValue a) => a    -- the multiplicative identity
one = S.one
