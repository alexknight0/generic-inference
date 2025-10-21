{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{- | Contains functions for computing inference using solution extension sets.

Currently incomplete: only contains functions that can operate on quasi-regular valuation algebras.
-}
module LocalComputation.Inference.DynamicProgramming (
    solution
) where

import qualified Data.Map                                 as Map
import           Data.Proxy                               (Proxy (Proxy))
import qualified Data.Set                                 as S
import qualified LocalComputation.Inference.JoinTree      as JT
import qualified LocalComputation.Inference.JoinTree.Tree as JT
import qualified LocalComputation.ValuationAlgebra        as V


-- TODO: Could add an assert on the given tree like 'supportsCollect' but only the renumbered part.

-- | Computes a single solution from the solution set by repeatedly extending a solution set
-- until it encompasses the whole query. In this case of a quasiregular valuation there is
-- only one possible solution, so this algorithm finds the only solution.
--
-- Based off the pseudocode for algorithm 8.3 in Marc Pouly's "Generic Inference".
-- Note: in the pseudocode for algorithm 8.3, the 'r' subscript of psi indicates that
-- the 'fusion algorithm' was executed with 'r' as the root node.
solution :: forall v a . (V.Valuation v a)
    => JT.JoinTree v a
    -> V.VarAssignment v a
solution t = go (t.root.id - 1) initialX
    where
        vertices = JT.vertexMap t

        initialX = S.findMin $ V.configurationExtSet t.root.v (V.emptyAssignment (Proxy @(v a)))

        go 0 x = x
        go i x
            | V.isIdentity nodeI.v = go (i - 1) x -- Skip identity elements
                                                  -- (extension sets not implemented for them)
            | otherwise            = go (i - 1) (V.combineAssignments (Proxy @(v a)) x y)
            where
                nodeI      = (Map.!) vertices i
                nodeChildI = head $ JT.unsafeOutgoingEdges nodeI.id t

                y = S.findMin $ V.configurationExtSet nodeI.v (V.projectAssignment (Proxy @(v a)) x intersection)

                intersection = S.intersection nodeI.d nodeChildI.d

