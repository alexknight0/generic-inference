{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LocalComputation.Inference.JoinTree.Forest (
    -- Join tree type
    -- TODO: remove g?
      JoinForest (g)

    -- Join tree functions
    , fromGraph
    , unsafeFindById
    , unsafeOutgoingEdges
    , unsafeIncomingEdges
    , unsafeOutgoingEdges'
    , unsafeIncomingEdges'
    , neighbourMap
    , vertexList
    , mapVertices
    , unsafeConvertToCollectTree
    , toForest
) where

import           GHC.Records                              (HasField, getField)
import           LocalComputation.ValuationAlgebra        hiding
                                                          (assertInvariants,
                                                           satisfiesInvariants)

import qualified Algebra.Graph                            as G
import qualified Algebra.Graph.Acyclic.AdjacencyMap       as G (toAcyclic)
import qualified Algebra.Graph.ToGraph                    as G (dfsForest,
                                                                reachable,
                                                                toAdjacencyMap,
                                                                topSort)
import qualified Algebra.Graph.Undirected                 as UG
import           Control.Exception                        (assert)
import qualified Data.Bifunctor                           as B
import qualified Data.List                                as L
import qualified Data.Map                                 as M
import           Data.Maybe                               (fromJust, isJust)
import qualified Data.Set                                 as S
import           Data.Text.Lazy                           (unpack)
import qualified LocalComputation.Utils                   as U
import           Numeric.Natural                          (Natural)
import           Text.Pretty.Simple                       (pShow)

import           LocalComputation.Inference.JoinTree.Tree (Id, JoinTree,
                                                           Node (id),
                                                           NodeType (..))
import qualified LocalComputation.Inference.JoinTree.Tree as JT

--------------------------------------------------------------------------------
-- Join Trees
--------------------------------------------------------------------------------

-- TODO: Invariants! For example we should know that this is a forest at every point in time.
-- (this implies everything is a tree!!!)

-- TODO: Add invaraint that it can't be empty to make `root` safe.
newtype JoinForest v = UnsafeJoinForest { g :: G.Graph (Node v) }

-- TODO: Is this used??? Should it be?
instance HasField "root" (JoinForest v) (Node v) where
    getField t = L.maximumBy (\x y -> x.id `compare` y.id) $ G.vertexList t.g

fromGraph :: G.Graph (Node v) -> JoinForest v
fromGraph = U.assertP satisfiesInvariants . UnsafeJoinForest

findById :: Id -> JoinForest v -> Maybe (Node v)
findById i t = L.find (\n -> n.id == i) $ G.vertexList t.g

toForest :: JoinTree v -> JoinForest v
toForest t = fromGraph $ JT.unsafeGetGraph t

--------------------------------------------------------------------------------
-- Transformations
--------------------------------------------------------------------------------

-- TODO: Should probably not be exposed; runs risk of ruining running intersection property or node labelling.

-- | Flips an edge from x to y such that it now goes from y to x.
-- TODO: Should probably not be exposed; runs risk of ruining running intersection property or node labelling.
mapVertices :: (Node a -> Node b) -> JoinForest a -> JoinForest b
mapVertices f t = fromGraph $ fmap f t.g

--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------
trees :: forall v . JoinForest v -> [JoinTree v]
trees t = getTrees' (vertexSet t)

    where
        undirected = G.overlay (t.g) (G.transpose t.g)

        getTrees' :: S.Set (Node v) -> [JoinTree v]
        getTrees' vertices
            | length vertices == 0 = []
            | otherwise            = newTree : getTrees' (S.difference vertices verticesInNewTree)

            where
                -- Take a random vertex out
                vertexInNewTree = S.findMin vertices

                -- Take out all the vertices in its tree
                verticesInNewTree = S.fromList $ G.reachable undirected vertexInNewTree

                -- Get the tree
                newTree = JT.fromGraph $ G.induce (\n -> n `elem` verticesInNewTree) t.g


-- | A sorted list of vertices of the forest; equivalent to a topological ordering.
vertexList :: JoinForest v -> [Node v]
vertexList t = G.vertexList t.g

vertexSet :: JoinForest v -> S.Set (Node v)
vertexSet t = G.vertexSet t.g

neighbourMap :: JoinForest v -> M.Map Id [Node v]
neighbourMap t = M.fromList . map (B.first (.id)) . UG.adjacencyList . UG.toUndirected $ t.g

incomingEdges :: Id -> JoinForest v -> Maybe [Node v]
incomingEdges = (fmap snd .) . incomingEdges'

incomingEdges' :: Id -> JoinForest v -> Maybe (Node v, [Node v])
incomingEdges' i t = JT.outgoingGraphEdges i . G.transpose $ t.g

outgoingEdges :: Id -> JoinForest v -> Maybe [Node v]
outgoingEdges = (fmap snd .) . outgoingEdges'

outgoingEdges' :: Id -> JoinForest v -> Maybe (Node v, [Node v])
outgoingEdges' i t = L.find (\(n, _) -> n.id == i) . G.adjacencyList $ t.g

--------------------------------------------------------------------------------
-- Collect tree creation
--------------------------------------------------------------------------------
unsafeConvertToCollectTree :: forall v a . (Show a, Valuation v, Ord a)
    => JoinForest (v a)
    -> Domain a
    -> JoinTree (v a)
unsafeConvertToCollectTree f q = U.assertP JT.supportsCollect $ JT.redirectToQueryNode q treeWithQueryNode
    where
        treeWithQueryNode :: JoinTree (v a)
        treeWithQueryNode = fromJust $ L.find (\t -> isJust $ L.find (\n -> n.d == q)
                                                                     (JT.vertexList t))
                                              (trees f)

--------------------------------------------------------------------------------
-- Invariants
--------------------------------------------------------------------------------
satisfiesInvariants :: JoinForest v -> Bool
satisfiesInvariants f = all JT.satisfiesInvariants (trees f)

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



