{-# LANGUAGE ScopedTypeVariables #-}

{- | Contains functions for computing inference using solution extension sets.

Currently incomplete: only contains functions that can operate on quasi-regular valuation algebras.
-}
module LocalComputation.Inference.DynamicProgramming (
    solution
) where

import           Control.Exception                              (assert)
import qualified Data.Map                                       as Map
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
solution :: forall a b . (V.Var a, Q.SemiringValue b, Show b, Eq b)
    => JT.JoinTree (Q.Valuation b a)
    -> V.VarAssignment (Q.Valuation b) a b
solution t | assert (JT.supportsCollect t) False = undefined
solution t = go (t.root.id - 1) initialX
    where
        vertices = JT.vertexMap t

        initialX = S.findMin $ Q.configExtSet t.root.v empty

        empty = M.reshape U.unusedArg M.empty S.empty (S.singleton ())

        go 0 x = x
        go i x
            | V.isIdentity nodeI.v = go (i - 1) x   -- Skip identity elements
            | otherwise            = go (i - 1) (M.unsafeAppendRows x y)
            where
                nodeI      = (Map.!) vertices i
                nodeChildI = head $ JT.unsafeOutgoingEdges nodeI.id t

                y = S.findMin $ Q.configExtSet nodeI.v (M.unsafeProjectRows x intersection)

                intersection = S.intersection nodeI.d nodeChildI.d

