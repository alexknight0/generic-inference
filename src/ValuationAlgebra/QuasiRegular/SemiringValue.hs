module ValuationAlgebra.QuasiRegular.SemiringValue
    ( QuasiRegularSemiringValue (quasiInverse)
    , SemiringValue (add, multiply, zero, one)
    )
where

import           ValuationAlgebra.Semiring

-- | A Quasi-regular semiring is also known as a 'closed' semiring
class (SemiringValue a) => QuasiRegularSemiringValue a where
    quasiInverse :: a -> a

    -- zero and one also has to satisfy some special laws with
    -- respect to idempotency and monotonicity to
    -- work for path problems? pdf page 257 gen. inf.
