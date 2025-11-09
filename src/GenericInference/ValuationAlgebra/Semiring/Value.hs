module GenericInference.ValuationAlgebra.Semiring.Value
    ( SemiringValue (add, multiply, zero, one)
    )
where

class SemiringValue a where
    add :: a -> a -> a
    multiply :: a -> a -> a
    zero :: a   -- the additive identity
    one :: a    -- the multiplicative identity
