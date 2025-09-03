{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LocalComputation.ValuationAlgebra.QuasiRegular
    ( Q.QSemiringValue (quasiInverse)
    , Q.SemiringValue (add, multiply, zero, one)
    , QuasiRegularValuation
    , create
    , solution
    , Q.TropicalSemiringValue (T)
    )
where

import           Control.Exception                                            (assert)
import           Data.Maybe                                                   (fromJust)
import qualified Data.Set                                                     as S
import qualified LocalComputation.LabelledMatrix                              as M
import           LocalComputation.ValuationAlgebra
import qualified LocalComputation.ValuationAlgebra.QuasiRegular.SemiringValue as Q

-- Typeclasses
import           Control.DeepSeq                                              (NFData)
import           Data.Binary                                                  (Binary)
import           GHC.Generics                                                 (Generic)

import qualified Algebra.Graph                                                as DG
import           Data.List                                                    (maximumBy)
import qualified Data.Map                                                     as Map
import           LocalComputation.Inference.JoinTree                          (Node (id, v))
import           LocalComputation.Inference.ShenoyShafer                      (InferredData)
import           LocalComputation.Utils                                       (findAssertSingleMatch,
                                                                               unsafeFind,
                                                                               unusedArg)
import qualified LocalComputation.Utils                                       as DG (neighbours)
import qualified LocalComputation.Utils                                       as U

type Foo a = (Binary a, NFData a, Generic a)

data QuasiRegularValuation b a = Valuation (M.LabelledMatrix a a b) (M.LabelledMatrix a () b) | Identity (Domain a) deriving (Binary, NFData, Ord, Eq, Generic, Show)

create :: (Var a, Show b, Q.QSemiringValue b) => M.LabelledMatrix a a b -> M.LabelledMatrix a () b -> Maybe (QuasiRegularValuation b a)
create m b
    | satisfiesInvariants (Valuation m b) = Just (Valuation m b)
    | otherwise = Nothing

instance (Show c, Q.QSemiringValue c) => Valuation (QuasiRegularValuation c) where

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
solution :: (Show a, Ord a, Show b, Q.QSemiringValue b) => QuasiRegularValuation b a -> M.LabelledMatrix a () b
solution (Valuation m b) = matrixMultiply (matrixQuasiInverse m) b
solution (Identity _)    = error "'solution' called on identity valuation."

-- | Adds two valuations. Unsafe.
add :: (Var a, Show b, Q.QSemiringValue b) => QuasiRegularValuation b a -> QuasiRegularValuation b a -> QuasiRegularValuation b a
add v1 v2 = assertInvariants $ _add v1 v2

_add :: (Var a, Show b, Q.QSemiringValue b) => QuasiRegularValuation b a -> QuasiRegularValuation b a -> QuasiRegularValuation b a
_add (Valuation m1 b1) (Valuation m2 b2) = fromJust $ create (matrixAdd m1 m2) (matrixAdd b1 b2)
_add _                 _                 = error "Not implemented error."

-- | Extends a valuation. Unsafe.
extension :: (Var a, Show b, Q.QSemiringValue b) => QuasiRegularValuation b a -> S.Set a -> QuasiRegularValuation b a
extension v d = assertInvariants $ _extension v d

_extension :: (Var a, Show b, Q.QSemiringValue b) => QuasiRegularValuation b a -> S.Set a -> QuasiRegularValuation b a
_extension (Identity _) d = Identity d
_extension (Valuation m b) t = fromJust $ create (fromJust $ M.extension m t t Q.zero) (fromJust $ M.extension b t (S.singleton ()) Q.zero)


------------------------------------------------------------------------------
-- QuasiRegularValuation Extension Sets
------------------------------------------------------------------------------

-- TODO: should we replace the operations of project here with variable elimination?
--
-- | Produces the configuration set. See page 368 of Marc Pouly's "Generic Inference"
configSet :: (Q.QSemiringValue c, Show a, Show c, Ord a)
    => QuasiRegularValuation c a
    -> Domain a
    -> M.LabelledMatrix a () c
    -> Maybe (S.Set (M.LabelledMatrix a () c))
configSet     (Identity _)    _ _ = error "Not implemented error"
configSet phi@(Valuation m b) t x = Just $ S.singleton result
    where
        result = matrixMultiply (matrixQuasiInverse (matrixProject m sMinusT sMinusT))
                                (matrixAdd (matrixMultiply (matrixProject m sMinusT t)
                                                           (x)
                                            )
                                           (matrixProject b sMinusT (S.singleton ())))

        s = label phi
        sMinusT = S.difference s t

getValuation :: (Eq a) => a -> [(a, b)] -> b
getValuation x results = snd $ unsafeFind (\(d, v) -> d == x) results

-- TODO: need child info.

-- TODO: We should take one of the following approaches
-- 1. Check if there is a way to perform this algorithm without the notion of labels
--      (might be hard because it seems to use child(..))
-- 2. Provide node id information inside inferred data so we don't have to stitch it back together afterward.
--      (If we get asserts failing here, we actually can't stitch it back together accurately!)
multiqueryCompute :: forall a c . (Eq a, Q.QSemiringValue c, Show a, Show c, Ord a, Ord c)
    => DG.Graph (Node ((QuasiRegularValuation c) a))
    -> InferredData (QuasiRegularValuation c) a
    -> S.Set (M.LabelledMatrix a () c)
multiqueryCompute g results = go rootNode.id rootConfigSet rootNode.d
    where
        vertices = DG.vertexList results

        rootNode :: Node ((QuasiRegularValuation c) a)
        rootNode = maximum vertices

        rootConfigSet :: Maybe (S.Set (M.LabelledMatrix a () c))
        rootConfigSet = configSet rootNode.v S.empty M.empty

        go i c s = undefined
            where
                -- This doesn't exactly follow the mathematical definition as doing so
                -- appears to require enumerating an infinite set.
                c' = undefined

                s' = S.union s phiI.d
                phiI = unsafeFind (\n -> n.id == i) vertices


-- the 'r' in the psi indicates that the 'fusion algorithm' was executed with 'r' as the root node.
singleSolutionCompute :: forall a c . (Eq a, Q.QSemiringValue c, Show a, Show c, Ord a, Ord c)
    => InferredData (QuasiRegularValuation c) a
    -> M.LabelledMatrix a () c
singleSolutionCompute g = go (rootNode.id - 1) initialX
    where
        vertices = DG.vertexList g

        rootNode :: Node ((QuasiRegularValuation c) a)
        rootNode = maximum vertices

        initialX = S.findMin $ fromJust $ configSet rootNode.v S.empty empty

        empty = M.reshape unusedArg M.empty S.empty (S.singleton ())

        go 0 x = x
        go i x = go (i - 1) (fromJust $ M.appendRows x y')
            where
                nodeI      = unsafeFind (\n -> n.id == i) vertices
                nodeChildI = head $ fromJust $ DG.neighbours nodeI g

                y' = S.findMin $ fromJust y

                y = configSet nodeI.v
                              intersectionOfIAndChildI
                              (matrixProject x intersectionOfIAndChildI (S.singleton ()))
                intersectionOfIAndChildI = (S.intersection nodeI.d nodeChildI.d)



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

