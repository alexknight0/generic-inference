module ValuationAlgebra.QuasiRegular.SemiringValue
    ( QuasiRegularSemiringValue (add, multiply, quasiInverse, zero, one)
    )
where

-- | A Quasi-regular semiring is also known as a 'closed' semiring
class QuasiRegularSemiringValue a where
    add :: a -> a -> a
    multiply :: a -> a -> a
    quasiInverse :: a -> a
    zero :: a  -- The additive identity
    one :: a   -- The multiplicative identity

    -- zero and one also has to satisfy some special laws with
    -- respect to idempotency and monotonicity to
    -- work for path problems? pdf page 257 gen. inf.

