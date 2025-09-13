{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LocalComputation.Inference.JoinTree.Tree (

    -- Nodes
      Node (id, v, t)
    , node
    , changeContent
    , NodeType (Valuation, Query, Union, Projection)
    , Id

    -- Join Trees
    , JoinTree (g)
    , satisfiesInvariants
    , unsafeFromGraph
    , supportsCollect
    , vertexList
    , redirectToQueryNode
    , unsafeOutgoingEdges
    , unsafeIncomingEdges

    -- Utilities
    , outgoingGraphEdges
    , renumberTopological
) where

import           GHC.Records                        (HasField, getField)
import           LocalComputation.ValuationAlgebra  hiding (assertInvariants,
                                                     satisfiesInvariants)

import qualified Algebra.Graph                      as G
import qualified Algebra.Graph.Acyclic.AdjacencyMap as G (toAcyclic)
import qualified Algebra.Graph.ToGraph              as G (isTopSortOf,
                                                          reachable,
                                                          toAdjacencyMap,
                                                          topSort)
import qualified Data.List                          as L
import qualified Data.Map                           as M
import           Data.Maybe                         (fromJust, isJust)
import           Data.Text.Lazy                     (unpack)
import qualified LocalComputation.Utils             as L (count)
import qualified LocalComputation.Utils             as U
import           Numeric.Natural                    (Natural)
import           Text.Pretty.Simple                 (pShow)

--------------------------------------------------------------------------------
-- Nodes
--------------------------------------------------------------------------------

data Node v = Node {
      id :: Id
    , v  :: v
    , t  :: NodeType
} deriving (Generic, Typeable, Binary)

type Id = Integer

data NodeType = Valuation | Query | Union | Projection deriving (Show, Generic, Binary, Enum, Bounded, Eq)

node :: Id -> v -> NodeType -> Node v
node = Node

changeContent :: Node a -> a -> Node a
changeContent n v = n { v = v }

-- | Accessor for the domain of the valuation.  Equivalent to calling `label` on the valuation.
-- __Warning__: Not necessarily O(1).
instance (Valuation v, Ord a, Show a) => HasField "d" (Node (v a)) (Domain a) where
    getField m = label m.v

-- | Equality of nodes defers to id.
instance Eq (Node v) where
    -- Equality is necessary for use with Algebra.Graph
    x == y = x.id == y.id

-- | Ordering of nodes defers to id.
-- In combination with the invariants of the join tree, a sorted list of vertices becomes
-- a topological ordering.
instance Ord (Node v) where
    -- An ordering is necessary for use with Algebra.Graph
    x <= y = x.id <= y.id

instance (Valuation v, Ord a, Show a) => Show (Node (v a)) where
    show n = unpack $ pShow (n.id, n.d)

--------------------------------------------------------------------------------
-- Join trees
--------------------------------------------------------------------------------

{- | A join tree is:
    1. a non-empty tree (connected, acyclic graph),
    2. such that the node 'id' fields form a topological ordering,
    3. it is directed toward a node called the 'root',
    4. and has the running intersection property.

    2 & 3 => root is the node with the largest id
    1 & 3 => root has no outgoing edges

Notably the numbering of nodes with ids may not be total - some numbers may be skipped.
For example, there may exist nodes with ids of 3 and 5 without the existence of a node of id 4.

For the definition of the running intersection property, see Marc Pouly's "Generic Inference".
-}
newtype JoinTree v = UnsafeJoinTree { g :: G.Graph (Node v) }

-- | Checks a given join tree satisfies the invariants (1), (2), (3), and (4)
-- specified in the declaration of the join tree.
satisfiesInvariants :: JoinTree v -> Bool
satisfiesInvariants t = vertexCount t > 0 && isAcyclic t          -- (1)
                            && hasTopologicalNumbering t          -- (2)
                            && isDirectedTowardsRoot t            -- (3)
                            && hasRunningIntersectionProperty t   -- (4)

instance HasField "root" (JoinTree v) (Node v) where
    getField t = last $ vertexList t

-- | Converts a graph into a join tree.
--
-- __Warning__: Unsafe - if assertions are enabled, will check some invariants associated with a join tree
-- and throw an error if the graph doesn't satisfy these invariants. If assertions are disabled, may
-- result in a malformed data structure.
unsafeFromGraph :: G.Graph (Node v) -> JoinTree v
unsafeFromGraph = U.assertP satisfiesInvariants . UnsafeJoinTree

--------------------------------------------------------------------------------
-- Transformations
--------------------------------------------------------------------------------

-- | Redirects a given join tree to reverse edges to face the node of the given id.
-- If a forest is given, will not impact trees that don't contain the node of the given id.
redirectTree :: forall a . Id -> JoinTree a -> JoinTree a
redirectTree i' t = unsafeFromGraph . renumberTopological . redirectTree' i' $ t.g
    where

        -- Works by traversing out from the given node,
        -- flipping any edges that it uses along its journey
        redirectTree' :: Id -> G.Graph (Node a) -> G.Graph (Node a)
        redirectTree' i g = foldr f g outgoingNodes
            where
                (this, outgoingNodes) = fromJust $ outgoingGraphEdges i g

                f :: Node a -> G.Graph (Node a) -> G.Graph (Node a)
                f n acc = flipEdge this n $ redirectTree' n.id acc

-- | Redirects the given join tree to reverse edges to face a query node of the given domain.
-- If multiple query nodes with this domain exist, one is chosen at random. This function only
-- searches amongst nodes with a `NodeType` of `Query`.
redirectToQueryNode :: (Valuation v, Ord a, Show a)
    => Domain a -> JoinTree (v a) -> JoinTree (v a)
redirectToQueryNode d g = redirectTree (queryNode.id) g
    where
        -- TODO: Update
        queryNode = head $ filter (\n -> n.d == d && n.t == Query) (vertexList g)

--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------
vertexCount :: JoinTree v -> Int
vertexCount t = G.vertexCount t.g

-- | Returns a sorted vertex list; equivalent to a topological ordering.
vertexList :: JoinTree v -> [Node v]
vertexList t = G.vertexList t.g

unsafeIncomingEdges :: Id -> JoinTree v -> [Node v]
unsafeIncomingEdges i t = snd . fromJust . outgoingGraphEdges i . G.transpose $ t.g

unsafeOutgoingEdges :: Id -> JoinTree v -> [Node v]
unsafeOutgoingEdges i t = snd . fromJust . outgoingGraphEdges i $ t.g

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

renumberTopological :: G.Graph (Node a) -> G.Graph (Node a)
renumberTopological g = fmap (\n -> changeId n $ (M.!) newNumbering n.id) g
    where
        topological = U.fromRight $ G.topSort $ G.toAdjacencyMap g
        newNumbering = M.fromList $ zip (map (.id) topological) [0..]

        changeId n newId = n { id = newId }


--------------------------------------------------------------------------------
-- Invariants
--------------------------------------------------------------------------------
isAcyclic :: JoinTree v -> Bool
isAcyclic t = isJust . G.toAcyclic . G.toAdjacencyMap $ t.g

-- TODO: Implement
hasRunningIntersectionProperty :: JoinTree v -> Bool
hasRunningIntersectionProperty _ = True

-- | Returns true if the given tree is directed towards the root node.
-- Assumes given tree is acyclic.
isDirectedTowardsRoot :: JoinTree v -> Bool
isDirectedTowardsRoot t = length canReachRoot == vertexCount t
    where
        canReachRoot = G.reachable (G.transpose $ t.g) t.root

hasTopologicalNumbering :: JoinTree v -> Bool
hasTopologicalNumbering t = G.isTopSortOf (G.vertexList t.g) t.g

--------------------------------------------------------------------------------
-- Collect tree invariants
--------------------------------------------------------------------------------
isQueryNodeRoot :: JoinTree v -> Bool
isQueryNodeRoot t = t.root.t == Query

numQueryNodes :: JoinTree v -> Natural
numQueryNodes t = L.count (\n -> n.t == Query) $ vertexList t

hasTotalNumbering :: JoinTree v -> Bool
hasTotalNumbering t = t.root.id == fromIntegral (vertexCount t - 1)

{- | A join tree that 'supports collect' has the additional invariants that:
    1. the tree only has one query node,
    2. the query node is the root node,
    3. and the node id numbering is total.

A *total node id numbering* refers to the idea that for a given tree `t` every `Id`
between between 0 and `t.root.id` is in use by a node.
-}
supportsCollect :: JoinTree v -> Bool
supportsCollect t = numQueryNodes t == 1        -- (1)
                        && isQueryNodeRoot t    -- (2)
                        && hasTotalNumbering t  -- (3)

