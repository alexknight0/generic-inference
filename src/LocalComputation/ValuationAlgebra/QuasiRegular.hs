{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module LocalComputation.ValuationAlgebra.QuasiRegular
    ( Q.QSemiringValue (quasiInverse)
    , Q.SemiringValue (add, multiply, zero, one)
    , Valuation
    , create
    , solution
    , Q.TropicalSemiringValue (T)
    , configExtSet
    )
where

import qualified Data.Map                                                     as M
import           Data.Maybe                                                   (fromJust)
import qualified Data.Set                                                     as S
import qualified LocalComputation.LabelledMatrix                              as M
import           LocalComputation.ValuationAlgebra
import qualified LocalComputation.ValuationAlgebra.QuasiRegular.SemiringValue as Q


data Valuation b a = Valuation (M.LabelledMatrix a a b) (M.LabelledMatrix a () b) | Identity (Domain a) deriving (Binary, NFData, Ord, Eq, Generic)

instance (Show b, Show a) => Show (Valuation b a) where
    show (Identity _)    = "Identity"
    show (Valuation m b) = show m ++ "\n" ++ show b

create :: (Var a, Show b, Q.QSemiringValue b) => M.LabelledMatrix a a b -> M.LabelledMatrix a () b -> Maybe (Valuation b a)
create m b
    | satisfiesInvariants (Valuation m b) = Just (Valuation m b)
    | otherwise = Nothing

-- TODO: Probably can remove instance of Show? It doens't contribute to the 'label', 'combine', 'project' functionality no?
instance (Show b, Q.QSemiringValue b) => ValuationFamily (Valuation b) where

    type VarAssignment (Valuation b) a b = M.LabelledMatrix a () b

    label (Identity d)    = d
    label (Valuation _ b) = fst (M.domain b)

    _combine (Identity d) x = extension x (S.union (label x) d)
    _combine x (Identity d) = extension x (S.union (label x) d)
    _combine v1 v2 = add (extension v1 sUnionT) (extension v2 sUnionT)
        where
            sUnionT = S.union (label v1) (label v2)

    _project (Identity _) newD = Identity newD
    _project (Valuation m b) t = fromJust $ create newM newB
        where
            newM = matrixAdd (matrixProject m t t)
                             (matrixMultiply (x)
                                             (matrixProject m sMinusT t))
            newB = matrixAdd (matrixProject b t (S.singleton ()))
                             (matrixMultiply (x)
                                             (matrixProject b sMinusT (S.singleton ())))

            x = matrixMultiply (matrixProject m t sMinusT)
                               (matrixQuasiInverse $ matrixProject m sMinusT sMinusT)
            s = fst $ M.domain m
            sMinusT = S.difference s t

    identity d = Identity d

    satisfiesInvariants (Identity _) = True
    satisfiesInvariants (Valuation m b) = (M.isSquare m) && ((fst $ M.domain m) == (fst $ M.domain b)) && M.isWellFormed m && M.isWellFormed b


-- | Returns a product useful for the solution of fixpoint systems. Detailed page 367 of "Generic Inference" (Pouly & Kohlas, 2012)
solution :: (Show a, Ord a, Show b, Q.QSemiringValue b) => Valuation b a -> M.LabelledMatrix a () b
solution (Valuation m b) = matrixMultiply (matrixQuasiInverse m) b
solution (Identity _)    = error "'solution' called on identity valuation."

-- | Adds two valuations. Unsafe.
add :: (Var a, Show b, Q.QSemiringValue b) => Valuation b a -> Valuation b a -> Valuation b a
add v1 v2 = assertInvariants $ _add v1 v2

_add :: (Var a, Show b, Q.QSemiringValue b) => Valuation b a -> Valuation b a -> Valuation b a
_add (Valuation m1 b1) (Valuation m2 b2) = fromJust $ create (matrixAdd m1 m2) (matrixAdd b1 b2)
_add _                 _                 = error "Not implemented error."

-- | Extends a valuation. Unsafe.
extension :: (Var a, Show b, Q.QSemiringValue b) => Valuation b a -> S.Set a -> Valuation b a
extension v d = assertInvariants $ _extension v d

_extension :: (Var a, Show b, Q.QSemiringValue b) => Valuation b a -> S.Set a -> Valuation b a
_extension (Identity _) d = Identity d
_extension (Valuation m b) t = fromJust $ create (fromJust $ M.extension m t t Q.zero) (fromJust $ M.extension b t (S.singleton ()) Q.zero)


------------------------------------------------------------------------------
-- Valuation Extension Sets
------------------------------------------------------------------------------

-- | Produces the configuration extension set.
--
-- A configuration set is
-- See page 368 of Marc Pouly's "Generic Inference"
configExtSet :: (Q.QSemiringValue c, Show a, Show c, Ord a)
    => Valuation c a
    -> Domain a
    -> VarAssignment (Valuation c) a c
    -> S.Set (VarAssignment (Valuation c) a c)
configExtSet     (Identity _)    _ _ = error "Not implemented error"
configExtSet phi@(Valuation m b) t x = S.singleton result
    where
        result = matrixMultiply (matrixQuasiInverse (matrixProject m sMinusT sMinusT))
                                (matrixAdd (matrixMultiply (matrixProject m sMinusT t)
                                                           (x)
                                            )
                                           (matrixProject b sMinusT (S.singleton ())))

        s = label phi
        sMinusT = S.difference s t

------------------------------------------------------------------------------
-- Unsafe & quasiregular variants of matrix operations.                     --
------------------------------------------------------------------------------

matrixQuasiInverse :: (Show a, Ord a, Show c, Q.QSemiringValue c) => M.LabelledMatrix a a c -> M.LabelledMatrix a a c
matrixQuasiInverse = fromJust . M.quasiInverse

matrixProject :: (Ord a, Ord b) => M.LabelledMatrix a b c -> S.Set a -> S.Set b -> M.LabelledMatrix a b c
matrixProject = ((fromJust .) .) . M.project

matrixAdd :: (Ord a, Ord b, Q.QSemiringValue c) => M.LabelledMatrix a b c -> M.LabelledMatrix a b c -> M.LabelledMatrix a b c
matrixAdd = (fromJust .) . M.add Q.add

matrixMultiply :: (Eq a, Eq b, Eq c, Q.QSemiringValue d) => M.LabelledMatrix a b d -> M.LabelledMatrix b c d -> M.LabelledMatrix a c d
matrixMultiply = (fromJust .) . M.multiply Q.zero Q.add Q.multiply

