{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LocalComputation.ValuationAlgebra.QuasiRegular.SemiringValue
    ( QuasiRegularSemiringValue (quasiInverse)
    , SemiringValue (zero, one)
    , TropicalSemiringValue (T)
    , toDouble
    )
where

import           LocalComputation.ValuationAlgebra.Semiring

-- Typeclasses
import           Control.DeepSeq                            (NFData)
import           Data.Binary                                (Binary)
import           Debug.Pretty.Simple                        (pTrace)
import           GHC.Generics                               (Generic)

-- | A Quasi-regular semiring is also known as a 'closed' semiring
class (SemiringValue a) => QuasiRegularSemiringValue a where
    quasiInverse :: a -> a

    -- zero and one also has to satisfy some special laws with
    -- respect to idempotency and monotonicity to
    -- work for path problems? pdf page 257 gen. inf.

-- | A value from the tropical semiring of all real numbers. Detailed page 232 of "Generic Inference" (Pouly & Kohlas, 2012).
newtype TropicalSemiringValue = T Double deriving (Binary, Show, NFData, Eq, Generic, Read)

toDouble :: TropicalSemiringValue -> Double
toDouble (T x) = x

instance Num TropicalSemiringValue where
    (+) (T x) (T y) = T $ pTrace ("(+) " ++ show x ++ " " ++ show y) $ min x y
    (*) (T x) (T y) = T $ pTrace ("(*) " ++ show x ++ " " ++ show y) $ x + y
    (-) (T x) (T y) = error "undefined (-)"      -- T $ x - y
    abs       (T x) = error "undefined (abs)"    -- T $ abs x
    signum    (T x) = error "undefined (signum)" -- T $ signum x
    fromInteger x   = T $ fromInteger x

instance Ord TropicalSemiringValue where
    (T x) <= (T y) = x <= y

instance SemiringValue TropicalSemiringValue where
    zero = T (read "Infinity" :: Double)
    one = 0

instance QuasiRegularSemiringValue TropicalSemiringValue where
    quasiInverse x
        | x >= 0 = 0
        | otherwise = T (read "-Infinity" :: Double)


