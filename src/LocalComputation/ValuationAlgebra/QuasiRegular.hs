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
    , singleSolutionCompute
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
import           Debug.Trace                                                  (trace,
                                                                               traceShowId)
import           LocalComputation.Inference.JoinTree                          (Node (id, v))
import qualified LocalComputation.Inference.JoinTree                          as JT
import           LocalComputation.Inference.ShenoyShafer                      (InferredData)
import           LocalComputation.Utils                                       (findAssertSingleMatch,
                                                                               unsafeFind,
                                                                               unusedArg)
import qualified LocalComputation.Utils                                       as DG (neighbours)
import qualified LocalComputation.Utils                                       as U

type Foo a = (Binary a, NFData a, Generic a)

data QuasiRegularValuation b a = Valuation (M.LabelledMatrix a a b) (M.LabelledMatrix a () b) | Identity (Domain a) deriving (Binary, NFData, Ord, Eq, Generic)

instance (Show b, Show a) => Show (QuasiRegularValuation b a) where
    show (Identity _)    = "Identity"
    show (Valuation m b) = show m ++ "\n" ++ show b

create :: (Var a, Show b, Q.QSemiringValue b) => M.LabelledMatrix a a b -> M.LabelledMatrix a () b -> Maybe (QuasiRegularValuation b a)
create m b
    | satisfiesInvariants (Valuation m b) = Just (Valuation m b)
    | otherwise = Nothing

-- TODO: Probably can remove instance of Show? It doens't contribute to the 'label', 'combine', 'project' functionality no?
instance (Show b, Q.QSemiringValue b) => Valuation (QuasiRegularValuation b) where

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
        sMinusT = -- trace ("S_MINUS_T " ++ show (S.difference s t)) $
                    S.difference s t

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


-- TODO:
-- We were getting the correct solutions because:
--
--  1. In singleSolutionCompute the querynode is popped first (as we desire).
--     However the query node always contained the whole set of variables, so instead
--     of slowly building up the solution we got it in the first go.
--
--  2. We didn't have multiple graphs, which meant our join trees always got
--     constructed in such a way that we ended up getting the right result
--     I think.

-- TODO: NEED to fix ordering for this function as the 'go' function relies on ids.

-- TODO: I think the reason is we only actually have a very werid looking small graph.
-- Answer: nope?

-- TODO: Study the outputs of 'singleSolutionCompute' - namely how we are calling 'appendRow' on something
-- that already is the solution???????

-- the 'r' in the psi indicates that the 'fusion algorithm' was executed with 'r' as the root node.
singleSolutionCompute :: forall a b . (Var a, Q.QSemiringValue b, Show b, Ord b)
    => InferredData (QuasiRegularValuation b) a
    -> M.LabelledMatrix a () b
singleSolutionCompute g = --fromJust $ M.extension M.empty (initialX.rowLabelSet) (initialX.colLabelSet) Q.one
                        go (rootNode.id - 1) initialX
                        -- go (rootNode.id - 1) initialX
    where
        vertices = DG.vertexList g

        -- TODO: Fix.
        rootNode :: Node ((QuasiRegularValuation b) a)
        rootNode = -- U.assertP (isMax)  $
                    unsafeFind (\n -> n.t == JT.Query) vertices -- maximum $ U.assertP ((>0) . length) vertices

        isMax :: Node (QuasiRegularValuation b a) -> Bool
        isMax n = maximum vertices == n

        initialX = S.findMin $ fromJust $ configSet rootNode.v S.empty empty

        empty = M.reshape unusedArg M.empty S.empty (S.singleton ())

        go 0 x = x
        go i x = result
            where
                result = go (i - 1) (fromJust $ M.appendRows x y')
                msg = "------------------\n\
                      \i: " ++ show i ++ "\n\
                      \------------------\n\
                      \         X        \n\
                      \------------------\n\
                      \" ++ show x ++   "\
                      \------------------\n\
                      \         Y        \n\
                      \------------------\n\
                      \" ++ show y' ++  "\
                      \------------------\n"

                -- TODO: Does our join tree construction algorithm provide a graph that has a complete numbering?
                -- If not it's actually dead easy to ensure it does; we just have to renumber the nodes in a topological
                -- ordering (as join tree does for 'renumberTree')
                nodeI      = unsafeFind (\n -> n.id == i) vertices
                nodeChildI = head $ fromJust $ DG.neighbours nodeI g

                y' = S.findMin $ fromJust y

                y = -- trace ("-----------------\nPROJECTED\n---------------\n" ++ show projected) $
                    configSet nodeI.v
                              intersectionOfIAndChildI
                              projected
                    where
                        projected = matrixProject x intersectionOfIAndChildI (S.singleton ())
                intersectionOfIAndChildI = -- trace ("INTERSECTION: " ++ show result2)
                                            result2
                    where
                        result2 = S.intersection nodeI.d nodeChildI.d




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

