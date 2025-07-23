module LocalComputation.ValuationAlgebra.SemiringValue
    ( SemiringValue (zero, one)
    )
where

class (Num a) => SemiringValue a where
    zero :: a   -- the additive identity
    one :: a    -- the multiplicative identity
