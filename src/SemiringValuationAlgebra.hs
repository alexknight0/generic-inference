{-# LANGUAGE InstanceSigs #-}

module SemiringValuationAlgebra
    (
        SemiringValuation
    )
where

import ValuationAlgebra

data Variable a b = Variable {
      name :: a
    , possibleValues :: [b]
}


data SemiringValuation variable value = SemiringValuation [variable] [value]

instance Valuation (SemiringValuation variable) where
    label :: val var -> Domain var
    label = undefined

    combine :: val var -> val var -> val var
    combine = undefined

    project :: val var -> Domain var -> val var
    project = undefined







