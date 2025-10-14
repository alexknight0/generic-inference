{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LocalComputation.Inference.JoinTree.Forest (
    -- Join tree type
      JoinForest (g)

    -- Join tree functions
    , unsafeFromGraph
    , unsafeFindById
    , unsafeOutgoingEdges
    , unsafeIncomingEdges
    , unsafeOutgoingEdges'
    , unsafeIncomingEdges'
    , neighbourMap
    , vertexList
    , unsafeConvertToCollectTree
    , unsafeIsolateAndRenumber
    , toForest
    , unsafeToForest'
    , unsafeUpdateValuations
    , unsafeGetTree
    , treeList
    , treesWithQueryNodes
    , subTrees
) where

import           LocalComputation.ValuationAlgebra        hiding
                                                          (assertInvariants,
                                                           satisfiesInvariants)

import qualified Algebra.Graph.AdjacencyMap               as G
import qualified Algebra.Graph.ToGraph                    as G (ToGraph (toGraph),
                                                                reachable)
import qualified Algebra.Graph.Undirected                 as UG
import qualified Data.Bifunctor                           as B
import qualified Data.List                                as L
import qualified Data.Map                                 as M
import           Data.Maybe                               (fromJust, isJust)
import qualified Data.Set                                 as S
import qualified LocalComputation.Utils                   as U

import           Control.Exception                        (assert)
import           LocalComputation.Inference.JoinTree.Tree (Id, JoinTree,
                                                           Node (id))
import qualified LocalComputation.Inference.JoinTree.Tree as JT
import qualified LocalComputation.ValuationAlgebra        as V

--------------------------------------------------------------------------------
-- Join Trees
--------------------------------------------------------------------------------

newtype JoinForest v = UnsafeJoinForest { g :: G.AdjacencyMap (Node v) } deriving (V.Generic, V.NFData)

unsafeFromGraph :: Valuation v a => G.AdjacencyMap (Node (v a)) -> JoinForest (v a)
unsafeFromGraph = U.assertP satisfiesInvariants . UnsafeJoinForest

findById :: Id -> JoinForest v -> Maybe (Node v)
findById i t = L.find (\n -> n.id == i) $ G.vertexList t.g

-- TODO: rename unsafe
toForest :: Valuation v a => JoinTree (v a) -> JoinForest (v a)
toForest t = unsafeFromGraph t.g

unsafeToForest' :: Valuation v a => [JoinTree (v a)] -> JoinForest (v a)
unsafeToForest' ts = unsafeFromGraph $ G.overlays $ map (.g) ts

unsafeGetTree :: (Valuation v a) => JoinForest (v a) -> JoinTree (v a)
unsafeGetTree f = assert (treeCount f == 1) (JT.unsafeFromGraph f.g)

--------------------------------------------------------------------------------
-- Transformations
--------------------------------------------------------------------------------

-- | Updates valuations for nodes of the given ids.
--
-- __Warning__: Unsafe - asserts that for a given node, the domain of the new valuation
-- does not differ from the domain of the old valuation.
unsafeUpdateValuations :: (V.ValuationFamily v, Var a) => M.Map Id (v a) -> JoinForest (v a) -> JoinForest (v a)
unsafeUpdateValuations m t = unsafeFromGraph $ G.gmap f t.g
    where
        f n = case M.lookup n.id m of
                Nothing -> n
                Just v  -> assert (n.d == label v) $ n { JT.v = v }

-- | Returns the sub join trees that can be formed by traversing one edge from the root,
-- erasing the edge that was followed, and declaring the node visited the root of a valid
-- join tree.
subTrees :: Valuation v a => JoinTree (v a) -> [JoinTree (v a)]
subTrees t = treeList $ unsafeFromGraph $ G.removeVertex t.root t.g

--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------
treeCount :: (Valuation v a) => JoinForest (v a) -> Int
treeCount = length . treeList

treeList :: forall v a . (Valuation v a) => JoinForest (v a) -> [JoinTree (v a)]
treeList t = getTrees' (vertexSet t)

    where
        undirected = G.overlay (t.g) (G.transpose t.g)

        getTrees' :: S.Set (Node (v a)) -> [JoinTree (v a)]
        getTrees' vertices
            | length vertices == 0 = []
            | otherwise            = newTree : getTrees' (S.difference vertices verticesInNewTree)

            where
                -- Take a random vertex out
                vertexInNewTree = S.findMin vertices

                -- Take out all the vertices in its tree
                verticesInNewTree = S.fromList $ G.reachable undirected vertexInNewTree

                -- Get the tree
                newTree = JT.unsafeFromGraph $ G.induce (\n -> n `elem` verticesInNewTree) t.g

treesWithQueryNodes :: (Valuation v a) => JoinForest (v a) -> [JoinTree (v a)]
treesWithQueryNodes f = filter JT.hasQueryNode $ treeList f

-- | A sorted list of vertices of the forest; equivalent to a topological ordering.
vertexList :: JoinForest v -> [Node v]
vertexList t = G.vertexList t.g

vertexSet :: JoinForest v -> S.Set (Node v)
vertexSet t = G.vertexSet t.g

neighbourMap :: JoinForest v -> M.Map Id [Node v]
neighbourMap t = M.fromList . map (B.first (.id)) . UG.adjacencyList . UG.toUndirected $ G.toGraph $ t.g

incomingEdges :: Id -> JoinForest v -> Maybe [Node v]
incomingEdges = (fmap snd .) . incomingEdges'

incomingEdges' :: Id -> JoinForest v -> Maybe (Node v, [Node v])
incomingEdges' i t = JT.outgoingGraphEdges i . G.transpose $ t.g

outgoingEdges :: Id -> JoinForest v -> Maybe [Node v]
outgoingEdges = (fmap snd .) . outgoingEdges'

outgoingEdges' :: Id -> JoinForest v -> Maybe (Node v, [Node v])
outgoingEdges' i t = JT.outgoingGraphEdges i t.g

--------------------------------------------------------------------------------
-- Collect tree creation
--------------------------------------------------------------------------------
unsafeConvertToCollectTree :: forall v a . (Show a, ValuationFamily v, Ord a)
    => JoinForest (v a)
    -> Domain a
    -> JoinTree (v a)
unsafeConvertToCollectTree f q = U.assertP JT.supportsCollect $ JT.redirectToQueryNode q treeWithQueryNode
    where
        treeWithQueryNode :: JoinTree (v a)
        treeWithQueryNode = fromJust $ L.find (\t -> isJust $ L.find (\n -> n.d == q)
                                                                     (JT.vertexList t))
                                              (treeList f)

unsafeIsolateAndRenumber :: (ValuationFamily v, Var a)
    => JoinForest (v a) -> Domain a -> JoinTree (v a)
unsafeIsolateAndRenumber f q = JT.unsafeFromGraph . JT.renumberTopological $ treeWithQueryNode.g
    where
        treeWithQueryNode = fromJust $ L.find (\t -> isJust $ L.find (\n -> n.d == q)
                                                                     (JT.vertexList t))
                                              (treeList f)


--------------------------------------------------------------------------------
-- Invariants
--------------------------------------------------------------------------------
-- TODO: add fact that no variables in any of the join trees are shared.
-- TODO: Should we even ever have a join forest? Isn't there always a way to form
-- a tree from a join forest?
satisfiesInvariants :: Valuation v a => JoinForest (v a) -> Bool
satisfiesInvariants f = all JT.satisfiesInvariants (treeList f)

--------------------------------------------------------------------------------
-- Unsafe variants
--------------------------------------------------------------------------------

unsafeFindById :: Id -> JoinForest v -> Node v
unsafeFindById = (fromJust . ) . findById

unsafeIncomingEdges :: Id -> JoinForest v -> [Node v]
unsafeIncomingEdges = (fromJust .) . incomingEdges

unsafeIncomingEdges' :: Id -> JoinForest v -> (Node v, [Node v])
unsafeIncomingEdges' = (fromJust .) . incomingEdges'

unsafeOutgoingEdges :: Id -> JoinForest v -> [Node v]
unsafeOutgoingEdges = (fromJust .) . outgoingEdges

unsafeOutgoingEdges' :: Id -> JoinForest v -> (Node v, [Node v])
unsafeOutgoingEdges' = (fromJust .) . outgoingEdges'



