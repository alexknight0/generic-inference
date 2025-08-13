{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LocalComputation.ValuationAlgebra.QuasiRegular
    ( Q.QuasiRegularSemiringValue (quasiInverse)
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

-- TODO: Migrate to record syntax.
import qualified Algebra.Graph                                                as DG
import           Data.List                                                    (maximumBy)
import qualified Data.Map                                                     as Map
import           LocalComputation.Inference.JoinTree                          (Node (id, v))
import           LocalComputation.Inference.ShenoyShafer                      (InferredData)
import           LocalComputation.Utils                                       (findAssertSingleMatch,
                                                                               unsafeFind,
                                                                               unusedArg)
import qualified LocalComputation.Utils                                       as DG (neighbours)

data QuasiRegularValuation c a b = Valuation (M.LabelledMatrix a a c) (M.LabelledMatrix a () c) | Identity (Domain a) deriving (Binary, NFData, Ord, Eq, Generic, Show)

create :: (Eq a) => M.LabelledMatrix a a c -> M.LabelledMatrix a () c -> Maybe (QuasiRegularValuation c a b)
create m b
    | isWellFormed (Valuation m b) = Just (Valuation m b)
    | otherwise = Nothing

instance (Show c, Q.QuasiRegularSemiringValue c) => Valuation (QuasiRegularValuation c) where
    label x | assertIsWellFormed x = undefined
    label (Identity d)    = d
    label (Valuation _ b) = fst (M.domain b)

    combine x y | assertIsWellFormed x || assertIsWellFormed y = undefined
    combine (Identity d) x = extension x (S.union (label x) d)
    combine x (Identity d) = extension x (S.union (label x) d)
    combine v1 v2 = valuationAdd (extension v1 sUnionT) (extension v2 sUnionT)
        where
            sUnionT = S.union (label v1) (label v2)

    project x _ | assertIsWellFormed x = undefined
    project x y | assert (S.isSubsetOf y (label x)) False = undefined
    project (Identity _) newD = Identity newD
    project (Valuation m b) t = fromJust $ create newM newB
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

    eliminate x _ | assertIsWellFormed x = undefined
    eliminate v x = project v (S.difference (label v) x)

    -- TODO: Can't define this as need to provide a 'b' and we don't have access to a 'b'.
    -- Edit: Well we could define a 'maybe b' in the declaration... or a 'Left set | Right map'?

    -- frame x = S.singleton $ Map.fromList $ [(x, ()) | x <- S.toList $ label x]


-- | Returns a product useful for the solution of fixpoint systems. Detailed page 367 of "Generic Inference" (Pouly & Kohlas, 2012)
solution :: (Show a, Ord a, Show c, Q.QuasiRegularSemiringValue c) => QuasiRegularValuation c a b -> M.LabelledMatrix a () c
solution (Identity _)    = error "'solution' called on identity valuation."
solution (Valuation m b) = matrixMultiply (matrixQuasiInverse m) b

-- | Adds two valuations. Unsafe.
valuationAdd :: (Ord a, Q.QuasiRegularSemiringValue c) => QuasiRegularValuation c a b -> QuasiRegularValuation c a b -> QuasiRegularValuation c a b
valuationAdd x y | assertIsWellFormed x || assertIsWellFormed y = undefined
valuationAdd (Valuation m1 b1) (Valuation m2 b2) = fromJust $ create (matrixAdd m1 m2) (matrixAdd b1 b2)
valuationAdd _ _ = error "Not implemented error."  -- Not 100% certain on how to handle identity elements, but never called anyway.

-- | Extends a valuation. Unsafe.
extension :: (Ord a, Q.QuasiRegularSemiringValue c) => QuasiRegularValuation c a b -> S.Set a -> QuasiRegularValuation c a b
extension x _ | assertIsWellFormed x = undefined
extension (Identity _) d = Identity d
extension (Valuation m b) t = fromJust $ create (fromJust $ M.extension m t t Q.zero) (fromJust $ M.extension b t (S.singleton ()) Q.zero)

------------------------------------------------------------------------------
-- QuasiRegularValuation Extension Sets
------------------------------------------------------------------------------

-- TODO: should we replace the operations of project here with variable elimination?
--
-- | Produces the configuration set. See page 368 of Marc Pouly's "Generic Inference"
configSet :: (Q.QuasiRegularSemiringValue c, Show a, Show c, Ord a)
    => QuasiRegularValuation c a ()
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
multiqueryCompute :: forall a c . (Eq a, Q.QuasiRegularSemiringValue c, Show a, Show c, Ord a, Ord c)
    => DG.Graph (Node ((QuasiRegularValuation c) a ()))
    -> InferredData (QuasiRegularValuation c) a ()
    -> S.Set (M.LabelledMatrix a () c)
multiqueryCompute g results = go rootNode.id rootConfigSet rootNode.d
    where
        vertices = DG.vertexList results

        rootNode :: Node ((QuasiRegularValuation c) a ())
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
singleSolutionCompute :: forall a c . (Eq a, Q.QuasiRegularSemiringValue c, Show a, Show c, Ord a, Ord c)
    => InferredData (QuasiRegularValuation c) a ()
    -> M.LabelledMatrix a () c
singleSolutionCompute g = go (rootNode.id - 1) initialX
    where
        vertices = DG.vertexList g

        rootNode :: Node ((QuasiRegularValuation c) a ())
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

matrixQuasiInverse :: (Show a, Ord a, Show c, Q.QuasiRegularSemiringValue c) => M.LabelledMatrix a a c -> M.LabelledMatrix a a c
matrixQuasiInverse = fromJust . M.quasiInverse

matrixProject :: (Ord a, Ord b) => M.LabelledMatrix a b c -> S.Set a -> S.Set b -> M.LabelledMatrix a b c
matrixProject = ((fromJust .) .) . M.project

matrixAdd :: (Ord a, Ord b, Q.QuasiRegularSemiringValue c) => M.LabelledMatrix a b c -> M.LabelledMatrix a b c -> M.LabelledMatrix a b c
matrixAdd = (fromJust .) . M.add Q.add

matrixMultiply :: (Eq a, Eq b, Eq c, Q.QuasiRegularSemiringValue d) => M.LabelledMatrix a b d -> M.LabelledMatrix b c d -> M.LabelledMatrix a c d
matrixMultiply = (fromJust .) . M.multiply Q.zero Q.add Q.multiply

------------------------------------------------------------------------------
-- Asserts                                                                  --
------------------------------------------------------------------------------

isWellFormed :: (Eq a) => QuasiRegularValuation c a b -> Bool
isWellFormed (Identity _) = True
isWellFormed (Valuation m b) = (M.isSquare m) && ((fst $ M.domain m) == (fst $ M.domain b)) && M.isWellFormed m && M.isWellFormed b

assertIsWellFormed :: (Eq a) => QuasiRegularValuation c a b -> Bool
assertIsWellFormed x = assert (isWellFormed x) False

