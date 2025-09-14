{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Functions the creation and transformation of valid join trees.
--
-- A lot of the functions in this module are very inefficent, and could easily be implemented better.
-- Consider implementations that use a O(1) call for root, as this is used widely.
module LocalComputation.Inference.JoinTree.Tree (

    -- Nodes
      Node (id, v, t, postbox)
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
    , vertexMap
    , redirectToQueryNode
    , incomingEdges
    , incomingEdges'
    , outgoingEdges
    , outgoingEdges'
    , unsafeOutgoingEdges
    , unsafeIncomingEdges
    , toMap
    , toValuationMap
    , unsafeUpdateValuations

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
import qualified Algebra.Graph.Undirected           as UG
import qualified Data.Bifunctor                     as B
import qualified Data.List                          as L
import qualified Data.Map                           as M
import           Data.Maybe                         (fromJust, isJust,
                                                     isNothing)
import           Data.Text.Lazy                     (unpack)
import qualified LocalComputation.Utils             as L (count)
import qualified LocalComputation.Utils             as U
import           Numeric.Natural                    (Natural)
import           Text.Pretty.Simple                 (pShow)

--------------------------------------------------------------------------------
-- Nodes
--------------------------------------------------------------------------------

data Node v = Node {
      id      :: Id
    , v       :: v
    , t       :: NodeType
    , postbox :: Maybe (M.Map Id v)
} deriving (Generic, Typeable, Binary)

type Id = Integer

data NodeType = Valuation | Query | Union | Projection deriving (Show, Generic, Binary, Enum, Bounded, Eq)

node :: Id -> v -> NodeType -> Node v
node i v t = Node i v t Nothing

changeContent :: Node a -> a -> Node a
changeContent n v = n { v = v }

-- | Accessor for the domain of the valuation.  Equivalent to calling `label` on the valuation.
-- __Warning__: Not necessarily O(1).
instance (ValuationFamily v, Ord a, Show a) => HasField "d" (Node (v a)) (Domain a) where
    getField m = label m.v

-- | Equality of nodes defers to id.
instance Eq (Node v) where
    -- Equality is necessary for use with Algebra.Graph
    x == y = x.id == y.id

-- | Ordering of nodes defers to id.
-- In combination with the invariants of the join tree, a sorted list of vertices becomes
-- a topological ordering.
instance Ord (Node v) where
    -- An ordering is necessary for use with Algebra.Graph.
    -- Many things rely on this ordering; consider `vertexList` and `toMap`.
    x <= y = x.id <= y.id

instance (ValuationFamily v, Ord a, Show a) => Show (Node (v a)) where
    show n = unpack $ pShow (n.id, n.d)

--------------------------------------------------------------------------------
-- Join trees
--------------------------------------------------------------------------------

{- | A join tree is:
    1. a non-empty tree (connected, acyclic graph),
    2. such that the node 'id' fields form a topological ordering,
    3. it is directed toward a node called the 'root',
    4. and has the running intersection property.
    5. If a join tree node has a postbox, all join tree nodes have postboxes,
       and they only contain messages from their neighbours

    2 & 3 => root is the node with the largest id
    1 & 3 => root has no outgoing edges
    1 & 2 & 3 & 4 => Removing the root of the tree creates a forest of valid join
                     trees, each rooted by a neighbour to the original root
                     (used for `subTrees` included in the `Forest` module).

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
                            && verticesHaveValidPostbox t         -- (5)

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
redirectToQueryNode :: (ValuationFamily v, Ord a, Show a)
    => Domain a -> JoinTree (v a) -> JoinTree (v a)
redirectToQueryNode d g = redirectTree (queryNode.id) g
    where
        -- TODO: Update
        queryNode = head $ filter (\n -> n.d == d && n.t == Query) (vertexList g)

-- | Updates valuations for nodes of the given ids.
--
-- __Warning__: Unsafe - asserts that for a given node, the domain of the new valuation
-- does not differ from the domain of the old valuation.
unsafeUpdateValuations :: M.Map Id a -> JoinTree a -> JoinTree a
unsafeUpdateValuations m t = unsafeFromGraph $ fmap f t.g
    where
        f n = case M.lookup n.id m of
                Nothing -> n
                Just v  -> n { v = v }

--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------
vertexCount :: JoinTree v -> Int
vertexCount t = G.vertexCount t.g

-- | Returns a sorted vertex list; equivalent to a topological ordering.
vertexList :: JoinTree v -> [Node v]
vertexList t = G.vertexList t.g

vertexMap :: JoinTree v -> M.Map Id (Node v)
vertexMap t = M.fromList . map (\n -> (n.id, n)) $ vertexList t

incomingEdges :: Id -> JoinTree v -> Maybe [Node v]
incomingEdges = (fmap snd .) . incomingEdges'

incomingEdges' :: Id -> JoinTree v -> Maybe (Node v, [Node v])
incomingEdges' i t = outgoingGraphEdges i . G.transpose $ t.g

outgoingEdges :: Id -> JoinTree v -> Maybe [Node v]
outgoingEdges = (fmap snd .) . outgoingEdges'

outgoingEdges' :: Id -> JoinTree v -> Maybe (Node v, [Node v])
outgoingEdges' i t = outgoingGraphEdges i t.g

toMap :: JoinTree v -> M.Map Id (Node v)
toMap t = M.fromAscList . map (\n -> (n.id, n)) $ vertexList t

toValuationMap :: JoinTree v -> M.Map Id v
toValuationMap = fmap (.v) . toMap

neighbourMap :: JoinTree v -> M.Map Id [Node v]
neighbourMap t = M.fromList . map (B.first (.id)) . UG.adjacencyList . UG.toUndirected $ t.g

--------------------------------------------------------------------------------
-- Unsafe variants
--------------------------------------------------------------------------------

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

verticesHaveValidPostbox :: forall v . JoinTree v -> Bool
verticesHaveValidPostbox t = (allHavePostboxes || allDontHavePostboxes)
                                && all vertexHasValidPostbox (vertexList t)
    where
        allHavePostboxes     = all (\n -> isJust    n.postbox) $ vertexList t
        allDontHavePostboxes = all (\n -> isNothing n.postbox) $ vertexList t

        vertexHasValidPostbox :: Node v -> Bool
        vertexHasValidPostbox n = case n.postbox of
                                     Nothing -> True
                                     Just p  -> all (`elem` neighbours n) (M.keys p)

        neighbours :: Node v -> [Id]
        neighbours n = map (.id) $ (M.!) neighbourMap' n.id

        neighbourMap' = neighbourMap t

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

