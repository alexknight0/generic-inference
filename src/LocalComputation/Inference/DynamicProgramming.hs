{-# LANGUAGE ScopedTypeVariables #-}

{- | Contains functions for computing inference using solution extension sets.

Currently incomplete: only contains functions that can operate on quasi-regular valuation algebras.
-}
module LocalComputation.Inference.DynamicProgramming (
    solution
) where

import           Control.Exception                              (assert)
import           Data.Maybe                                     (fromJust)
import qualified Data.Set                                       as S
import qualified LocalComputation.Inference.JoinTree            as JT
import qualified LocalComputation.Inference.JoinTree.Tree       as JT
import qualified LocalComputation.LabelledMatrix                as M
import qualified LocalComputation.Utils                         as U
import qualified LocalComputation.ValuationAlgebra              as V
import qualified LocalComputation.ValuationAlgebra.QuasiRegular as Q


-- TODO: Have the function detect an invalid graph? (Bad numbering)

-- TODO: Update argument

-- | Computes a single solution from the solution set by repeatedly extending a solution set
-- until it encompasses the whole query. In this case of a quasiregular valuation there is
-- only one possible solution, so this algorithm finds the only solution.
--
-- Based off the pseudocode for algorithm 8.3 in Marc Pouly's "Generic Inference".
-- Note: in the pseudocode for algorithm 8.3, the 'r' subscript of psi indicates that
-- the 'fusion algorithm' was executed with 'r' as the root node.
--
-- This function can be made more generic; but dynamic programming is only implemented
-- for the quasiregular valuation algebra, so it is implemented instance-specific here.
solution :: forall a b . (V.Var a, Q.QSemiringValue b, Show b)
    => JT.JoinTree (Q.QuasiRegularValuation b a)
    -> M.LabelledMatrix a () b
solution g | assert (JT.supportsCollect g) False = undefined
solution g = go (rootNode.id - 1) initialX
    where
        vertices = JT.vertexList g

        rootNode :: JT.Node (Q.QuasiRegularValuation b a)
        rootNode = U.assertP isOnlyQueryNode $ maximum vertices

        isOnlyQueryNode :: JT.Node (Q.QuasiRegularValuation b a) -> Bool
        isOnlyQueryNode n = n.t == JT.Query && length (filter (\x -> x.t == JT.Query) vertices) == 1

        initialX = S.findMin $ fromJust $ Q.configSet rootNode.v S.empty empty

        empty = M.reshape U.unusedArg M.empty S.empty (S.singleton ())

        go 0 x = x
        go i x = go (i - 1) (fromJust $ M.appendRows x y')
            where
                -- TODO: Does our join tree construction algorithm provide a graph that has a complete numbering?
                -- If not it's actually dead easy to ensure it does; we just have to renumber the nodes in a topological
                -- ordering (as join tree does for 'renumberTree')
                nodeI      = U.unsafeFind (\n -> n.id == i) vertices
                nodeChildI = head $ JT.unsafeOutgoingEdges nodeI.id g

                y' = S.findMin $ fromJust y

                y = Q.configSet nodeI.v
                              intersectionOfIAndChildI
                              (M.unsafeProject x intersectionOfIAndChildI (S.singleton ()))

                intersectionOfIAndChildI = S.intersection nodeI.d nodeChildI.d

