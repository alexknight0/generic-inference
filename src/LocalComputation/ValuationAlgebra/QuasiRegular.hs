{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module LocalComputation.ValuationAlgebra.QuasiRegular
    ( Q.SemiringValue (quasiInverse)
    , Q.add, Q.multiply, Q.zero, Q.one
    , Valuation
    , create
    , unsafeCreate
    , solution
    , Q.TropicalSemiringValue (T)
    )
where

import           Data.Maybe                                           (fromJust)
import qualified Data.Set                                             as S
import qualified LocalComputation.LabelledMatrix                      as M
import qualified LocalComputation.Utils                               as U
import           LocalComputation.ValuationAlgebra                    hiding
                                                                      (Valuation)
import qualified LocalComputation.ValuationAlgebra                    as V
import qualified LocalComputation.ValuationAlgebra.QuasiRegular.Value as Q


data Valuation b a = Valuation (M.LabelledMatrix a a b) (M.LabelledMatrix a () b) | Identity deriving (Binary, NFData, Ord, Eq, Generic)

instance (Show b, Show a) => Show (Valuation b a) where
    show Identity        = "Identity"
    show (Valuation m b) = show m ++ "\n" ++ show b

create :: (Var a, Show b, Q.SemiringValue b) => M.LabelledMatrix a a b -> M.LabelledMatrix a () b -> Maybe (Valuation b a)
create m b
    | satisfiesInvariants (Valuation m b) = Just (Valuation m b)
    | otherwise = Nothing

unsafeCreate :: (Var a, Q.SemiringValue b, Show b) => M.LabelledMatrix a a b -> M.LabelledMatrix a () b -> Valuation b a
unsafeCreate m b = U.assertP satisfiesInvariants $ Valuation m b

instance (Show b, Q.SemiringValue b) => ValuationFamily (Valuation b) where

    type VarAssignment (Valuation b) a = M.LabelledMatrix a () b
    combineAssignments _ a1 a2 = M.unsafeAppendRows a1 a2
    projectAssignment _ a d    = M.unsafeProjectRows a d

    label Identity        = S.empty
    label (Valuation _ b) = fst (M.domain b)

    _combine Identity x = x
    _combine x Identity = x
    _combine v1 v2 = add (extension v1 sUnionT) (extension v2 sUnionT)
        where
            sUnionT = S.union (label v1) (label v2)

    _project Identity _ = Identity
    _project (Valuation m b) t = unsafeCreate newM newB
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

    identity _ = Identity

    isIdentity Identity = True
    isIdentity _        = False

    satisfiesInvariants Identity = True
    satisfiesInvariants (Valuation m b) = (M.isSquare m) && ((fst $ M.domain m) == (fst $ M.domain b)) && M.isWellFormed m && M.isWellFormed b

    configurationExtSet     Identity        _ = error "Not implemented error"
    configurationExtSet phi@(Valuation m b) x
        | S.null sMinusT = S.singleton empty   -- shortcut if nothing to extend
        | otherwise      = S.singleton result
        where
            result = matrixMultiply (matrixQuasiInverse (matrixProject m sMinusT sMinusT))
                                    (matrixAdd (matrixMultiply (matrixProject m sMinusT t)
                                                               (x)
                                                )
                                               (matrixProjectRows b sMinusT))

            t = x.rowLabelSet
            s = label phi
            sMinusT = S.difference s t

            empty = fromJust $ M.extension M.empty S.empty (S.singleton ()) U.unusedArg

    emptyAssignment _ = M.reshape U.unusedArg M.empty S.empty (S.singleton ())

    frameLength _ _ = V.Infinity


-- | Returns a product useful for the solution of fixpoint systems. Detailed page 367 of "Generic Inference" (Pouly & Kohlas, 2012)
solution :: (Show a, Ord a, Show b, Q.SemiringValue b) => Valuation b a -> M.LabelledMatrix a () b
solution (Valuation m b) = matrixMultiply (matrixQuasiInverse m) b
solution Identity        = error "'solution' called on identity valuation."

-- | Adds two valuations. Unsafe.
add :: (Var a, Show b, Q.SemiringValue b) => Valuation b a -> Valuation b a -> Valuation b a
add (Valuation m1 b1) (Valuation m2 b2) = unsafeCreate (matrixAdd m1 m2) (matrixAdd b1 b2)
add _                 _                 = error "Not implemented error."

-- | Extends a valuation. Unsafe.
extension :: (Var a, Show b, Q.SemiringValue b) => Valuation b a -> S.Set a -> Valuation b a
extension Identity _        = Identity
extension (Valuation m b) t = unsafeCreate (fromJust $ M.extension m t t Q.zero) (fromJust $ M.extension b t (S.singleton ()) Q.zero)

------------------------------------------------------------------------------
-- Valuation Extension Sets
------------------------------------------------------------------------------

-- TODO: NEXT go through this and `solution` inside `DynamicProgramming` and
-- add comments to help understanding. Then investigate how to get around the
-- 'identity' problem...


-- TODO: It seems unavoidable that we will have one large projection from the domain of
-- the parent node of the query in the join tree to the query. This is because the query
-- node is added to a union node, and then the path is flipped when the tree is rediected
-- to the query. However, I don't think this is an avoidable cost. Even if we build the join
-- tree with the query as the root, we at some point will need to get

------------------------------------------------------------------------------
-- Unsafe & quasiregular variants of matrix operations.                     --
------------------------------------------------------------------------------

matrixQuasiInverse :: (Show a, Ord a, Show c, Q.SemiringValue c) => M.LabelledMatrix a a c -> M.LabelledMatrix a a c
matrixQuasiInverse = M.unsafeQuasiInverse

-- If the label of a valuation matched the label of the node it is attached to in the join
-- tree then a `matrixProject` would only be called when the new domains the matrix is being
-- projected to are subsets of its existing domains. However, since that is not the case,
-- this operation is more of a reshape, where we project down to the new domains, and extend
-- the matrix if required to make sure it matches the new domains given.
matrixProject :: (Ord a, Ord b, Q.SemiringValue c)
    => M.LabelledMatrix a b c -> S.Set a -> S.Set b -> M.LabelledMatrix a b c
matrixProject m newD1 newD2 = extend $ M.unsafeProject m (S.intersection d1 newD1) (S.intersection d2 newD2)
    where
        (d1, d2) = M.domain m

        extend matrix = fromJust $ M.extension matrix newD1 newD2 Q.zero

matrixProjectRows :: (Ord a, Ord b, Q.SemiringValue c) => M.LabelledMatrix a b c -> S.Set a -> M.LabelledMatrix a b c
matrixProjectRows m newD = extend $ M.unsafeProjectRows m (S.intersection d newD)
    where
        (d, _) = M.domain m

        extend matrix = M.unsafeExtendRows matrix newD Q.zero

matrixAdd :: (Ord a, Ord b, Q.SemiringValue c) => M.LabelledMatrix a b c -> M.LabelledMatrix a b c -> M.LabelledMatrix a b c
matrixAdd = M.unsafeAdd Q.add

matrixMultiply :: (Eq a, Eq b, Eq c, Q.SemiringValue d) => M.LabelledMatrix a b d -> M.LabelledMatrix b c d -> M.LabelledMatrix a c d
matrixMultiply = M.unsafeMultiply Q.zero Q.add Q.multiply

