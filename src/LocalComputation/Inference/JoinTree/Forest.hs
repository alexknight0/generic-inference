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
    , topologicalOrdering
    , isForest
    , redirectToQueryNode
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

--------------------------------------------------------------------------------
-- Transformations
--------------------------------------------------------------------------------

-- TODO: This should really be a join tree method.

-- | Redirects a given join tree to reverse edges to face the node of the given id.
-- If a forest is given, will not impact trees that don't contain the node of the given id.
redirectTree :: forall a . Id -> JoinForest a -> JoinForest a
redirectTree i' t = fromGraph . renumberTree . redirectTree' i' $ t.g
    where

        -- Works by traversing out from the given node,
        -- flipping any edges that it uses along its journey
        redirectTree' :: Id -> G.Graph (Node a) -> G.Graph (Node a)
        redirectTree' i g = foldr f g outgoingNodes
            where
                (this, outgoingNodes) = fromJust $ outgoingGraphEdges i g

                f :: Node a -> G.Graph (Node a) -> G.Graph (Node a)
                f n acc = flipEdge this n $ redirectTree' n.id acc

        -- Renumber the tree to ensure a topological ordering.
        renumberTree :: G.Graph (Node a) -> G.Graph (Node a)
        renumberTree g = fmap (\n -> changeId n $ (M.!) newNumbering n.id) g
            where
                -- TODO: Fix.
                topological = U.assertP (\l -> (last l).t == Query) $ U.fromRight $ G.topSort $ G.toAdjacencyMap g
                newNumbering = M.fromList $ zip (map (.id) topological) [0..]

                changeId n newId = n { id = newId }

-- | Redirects the given join tree to reverse edges to face a query node of the given domain.
-- If multiple query nodes with this domain exist, one is chosen at random. This function only
-- searches amongst nodes with a `NodeType` of `Query`.
redirectToQueryNode :: (Valuation v, Ord a, Show a)
    => Domain a -> JoinForest (v a) -> JoinForest (v a)
redirectToQueryNode d g = redirectTree (queryNode.id) g
    where
        -- TODO: Update
        queryNode = head $ filter (\n -> n.d == d && n.t == Query) (vertexList g)


-- TODO: Should probably not be exposed; runs risk of ruining running intersection property or node labelling.

-- | Flips an edge from x to y such that it now goes from y to x.
-- TODO: Should probably not be exposed; runs risk of ruining running intersection property or node labelling.
mapVertices :: (Node a -> Node b) -> JoinForest a -> JoinForest b
mapVertices f t = fromGraph $ fmap f t.g

--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------

-- isQueryNodeRoot :: JoinForest v -> Bool
-- isQueryNodeRoot t = t.root.t == Query
--
-- numQueryNodes :: JoinForest v -> Natural
-- numQueryNodes t = L.genericLength $ filter (\n -> n.t == Query) $ vertexList t

-- A join tree is a DAG with the property that
-- every edge is directed towards the root node
-- (i.e. all vertices can reach the root node)
--
-- It also has the running intersection property...
-- but let's not test that here. It should hold since
-- we only really create trees through one construction algorithm,
-- and we never modify these trees by adding or removing an edge.
isForest :: JoinForest v -> Bool
isForest t = length (trees t) > 1

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


vertexList :: JoinForest v -> [Node v]
vertexList t = G.vertexList t.g

vertexSet :: JoinForest v -> S.Set (Node v)
vertexSet t = G.vertexSet t.g

neighbourMap :: JoinForest v -> M.Map Id [Node v]
neighbourMap t = M.fromList . map (B.first (.id)) . UG.adjacencyList . UG.toUndirected $ t.g

incomingEdges :: Id -> JoinForest v -> Maybe [Node v]
incomingEdges = (fmap snd .) . incomingEdges'

incomingEdges' :: Id -> JoinForest v -> Maybe (Node v, [Node v])
incomingEdges' i t = outgoingGraphEdges i . G.transpose $ t.g

outgoingEdges :: Id -> JoinForest v -> Maybe [Node v]
outgoingEdges = (fmap snd .) . outgoingEdges'

outgoingEdges' :: Id -> JoinForest v -> Maybe (Node v, [Node v])
outgoingEdges' i t = L.find (\(n, _) -> n.id == i) . G.adjacencyList $ t.g

topologicalOrdering :: JoinForest v -> [Node v]
topologicalOrdering t = U.fromRight $ G.topSort $ G.toAdjacencyMap t.g

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

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------
outgoingGraphEdges :: Id -> G.Graph (Node v) -> Maybe (Node v, [Node v])
outgoingGraphEdges i g = L.find (\(n, _) -> n.id == i) . G.adjacencyList $ g

flipEdge :: Node a -> Node a -> G.Graph (Node a) -> G.Graph (Node a)
flipEdge x y g
    | G.hasEdge x y g = addEdge y x $ G.removeEdge x y $ g
    | otherwise = g

    where
        addEdge :: a -> a -> G.Graph a -> G.Graph a
        addEdge x1 x2 g' = G.overlay (G.connect (G.vertex x1) (G.vertex x2)) g'



--------------------------------------------------------------------------------
-- Invariants
--------------------------------------------------------------------------------

satisfiesInvariants :: JoinForest v -> Bool
satisfiesInvariants f = all JT.satisfiesInvariants (trees f)





