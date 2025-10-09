{-# LANGUAGE CPP                 #-}
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
    , isQueryNode
    , NodeType (Valuation, Query, Union, Projection)
    , Id

    -- Join Trees
    , JoinTree (g)
    , satisfiesInvariants
    , unsafeFromGraph
    , supportsCollect
    , vertexCount
    , vertexList
    , hasQueryNode
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
    , toPostboxMap
    , unsafeUpdateValuations
    , unsafeUpdatePostboxes
    , verticesHavePostboxes
    , neighbourMap
    , treeWidth
    , tracking
    , trackMaxTreeWidth'
    , maxTreeWidthTracker
    , numUsedValuationsTracker
    , trackUsedValuations'

    -- Conversions
    , toTree
    , unsafeFromTree

    -- Utilities
    , outgoingGraphEdges
    , renumberTopological
) where

import           GHC.Records                        (HasField, getField)
import           LocalComputation.ValuationAlgebra  hiding (assertInvariants,
                                                     satisfiesInvariants)

import qualified Algebra.Graph.Acyclic.AdjacencyMap as G (toAcyclic)
import qualified Algebra.Graph.AdjacencyMap         as G
import qualified Algebra.Graph.ToGraph              as G (ToGraph (toGraph),
                                                          isTopSortOf,
                                                          reachable,
                                                          toAdjacencyMap,
                                                          topSort)
import qualified Algebra.Graph.Undirected           as UG
import           Control.Exception                  (assert)
import qualified Data.Bifunctor                     as B
import qualified Data.IORef                         as IO
import qualified Data.List                          as L
import qualified Data.Map                           as M
import           Data.Maybe                         (fromJust, isJust,
                                                     isNothing)
import qualified Data.Set                           as S
import           Data.Text.Lazy                     (unpack)
import qualified Data.Tree                          as T
import           GHC.Stack                          (HasCallStack)
import qualified LocalComputation.Utils             as L (count)
import qualified LocalComputation.Utils             as U
import qualified LocalComputation.ValuationAlgebra  as V
import           Numeric.Natural                    (Natural)
import qualified System.IO.Unsafe                   as IO
import           Text.Pretty.Simple                 (pShow)

--------------------------------------------------------------------------------
-- Nodes
--------------------------------------------------------------------------------

data Node v = Node {
      id      :: Id
    , v       :: v
    , t       :: NodeType
    , postbox :: Maybe (M.Map Id v)
} deriving (Generic, Typeable, Binary, NFData)

type Id = Integer

data NodeType = Valuation | Query | Union | Projection deriving (Show, Generic, Binary, Enum, Bounded, Eq, NFData)

node :: Id -> v -> NodeType -> Node v
node i v t = Node i v t Nothing

changeContent :: Node a -> a -> Node a
changeContent n v = n { v = v }

isQueryNode :: Node a -> Bool
isQueryNode n = n.t == Query

-- | Accessor for the domain of the valuation.  Equivalent to calling `label` on the valuation.
-- __Warning__: Not necessarily O(1).
instance (ValuationFamily v, Ord a, Show a) => HasField "d" (Node (v a)) (Domain a) where
    getField m = label m.v

-- | Equality of nodes defers to id.
instance Eq (Node v) where
    -- Equality is necessary for use with Algebra.Graph,
    -- and a lot of functions also utilise this property
    -- for easy 'is this the same node' checking.
    x == y = x.id == y.id

-- | Ordering of nodes defers to id.
-- In combination with the invariants of the join tree, a sorted list of vertices becomes
-- a topological ordering.
instance Ord (Node v) where
    -- An ordering is necessary for use with Algebra.Graph.
    -- Many things rely on this ordering; consider `vertexList` and `toMap`.
    x <= y = x.id <= y.id

instance (ValuationFamily v, Ord a, Show a) => Show (Node (v a)) where
    show n = unpack $ pShow (n.id, n.d, n.t)

--------------------------------------------------------------------------------
-- Join trees
--------------------------------------------------------------------------------

{- | A join tree is:
    1. a non-empty directed tree (connected, acyclic graph, with at most |V| - 1 edges),
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
newtype JoinTree v = UnsafeJoinTree { g :: G.AdjacencyMap (Node v) } deriving (NFData, Generic)

-- | Checks a given join tree satisfies the invariants (1), (2), (3), and (4)
-- specified in the declaration of the join tree.
satisfiesInvariants :: (V.ValuationFamily v, V.Var a) => JoinTree (v a) -> Bool
satisfiesInvariants t = vertexCount t > 0 && isTree t                -- (1)
                            && hasTopologicalNumbering t             -- (2)
                            && isConnectedAndDirectedTowardsRoot t   -- (3)
                            && hasRunningIntersectionProperty t      -- (4)
                            && verticesHaveValidPostbox t            -- (5)

instance HasField "root" (JoinTree v) (Node v) where
    getField t = last $ vertexList t

-- | Converts a graph into a join tree.
--
-- __Warning__: Unsafe - if assertions are enabled, will check some invariants associated with a join tree
-- and throw an error if the graph doesn't satisfy these invariants. If assertions are disabled, may
-- result in a malformed data structure.
unsafeFromGraph :: (HasCallStack, V.ValuationFamily v, V.Var a) => G.AdjacencyMap (Node (v a)) -> JoinTree (v a)
unsafeFromGraph = U.assertP satisfiesInvariants . UnsafeJoinTree

--------------------------------------------------------------------------------
-- Transformations
--------------------------------------------------------------------------------

-- | Redirects a given join tree to reverse edges to face the node of the given id.
-- If a forest is given, will not impact trees that don't contain the node of the given id.
redirectTree :: forall v a . (V.ValuationFamily v, V.Var a) => Id -> JoinTree (v a) -> JoinTree (v a)
redirectTree i' t = unsafeFromGraph . renumberTopological . redirectTree' i' $ t.g
    where

        -- Works by traversing out from the given node,
        -- flipping any edges that it uses along its journey
        redirectTree' :: Id -> G.AdjacencyMap (Node (v a)) -> G.AdjacencyMap (Node (v a))
        redirectTree' i g = foldr f g outgoingNodes
            where
                (this, outgoingNodes) = fromJust $ outgoingGraphEdges i g

                f :: Node (v a) -> G.AdjacencyMap (Node (v a)) -> G.AdjacencyMap (Node (v a))
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
unsafeUpdateValuations :: (V.ValuationFamily v, Var a) => M.Map Id (v a) -> JoinTree (v a) -> JoinTree (v a)
unsafeUpdateValuations m t = unsafeFromGraph $ G.gmap f t.g
    where
        f n = case M.lookup n.id m of
                Nothing -> n
                Just v  -> assert (n.d == label v) $ n { v = v }


-- | Updates postboxes for nodes of the given ids.
unsafeUpdatePostboxes :: (V.ValuationFamily v, Var a) => M.Map Id (Maybe (M.Map Id (v a))) -> JoinTree (v a) -> JoinTree (v a)
unsafeUpdatePostboxes m t = unsafeFromGraph $ G.gmap f t.g
    where
        f n = case M.lookup n.id m of
                Nothing -> n
                Just p  -> n { postbox = p }

--------------------------------------------------------------------------------
-- Conversions
--------------------------------------------------------------------------------
-- | Returns a data structure that may allow more efficent traversal of subtrees.
toTree :: JoinTree v -> T.Tree (Node v)
toTree t = toTree' (G.transpose t.g) t.root

-- | Creates a tree from a graph where each outgoing edge is to a child node (a subtree).
-- Assumes the graph is formed as a transposed join tree.
toTree' :: G.AdjacencyMap (Node v) -> Node v -> T.Tree (Node v)
toTree' g root = T.Node root subTrees
    where
        subTreeRoots = S.toList $ (M.!) (G.adjacencyMap g) root
        subTrees = map (toTree' g) subTreeRoots

unsafeFromTree :: (ValuationFamily v, Var a) => T.Tree (Node (v a)) -> JoinTree (v a)
unsafeFromTree t = unsafeFromGraph graph
    where
        graph = case t of
                    T.Node root [] -> G.vertex root             -- If single node, then won't show up in edge list.
                    _              -> G.edges $ edgesOfTree t

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

hasQueryNode :: JoinTree v -> Bool
hasQueryNode t = isJust $ L.find (\n -> n.t == Query) $ vertexList t

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

toPostboxMap :: JoinTree v -> M.Map Id (Maybe (M.Map Id v))
toPostboxMap = fmap (.postbox) . toMap

-- | Creates a mapping from a node id to its neighbours in the graph.
--
-- The neighbours list will not include the node itself
-- (no self loops possible in the join tree).
neighbourMap :: JoinTree v -> M.Map Id [Node v]
neighbourMap t = M.fromList . map (B.first (.id)) . UG.adjacencyList . UG.toUndirected $ G.toGraph $ t.g

-- | Returns true if all vertices have postboxes
--
-- A simple check due to the invariant that if a single vertex has a postbox,
-- all vertices must have postboxes.
verticesHavePostboxes :: JoinTree v -> Bool
verticesHavePostboxes t = isJust $ t.root.postbox

-- | Creates a mapping from a variable to the nodes in the tree that contain that variable
-- in their domain.
variableMap :: forall v a . (V.ValuationFamily v, V.Var a) => JoinTree (v a) -> M.Map a [Node (v a)]
variableMap t = foldr (M.unionWith (\x1 x2 -> x1 ++ x2)) M.empty $ map variableMapForNode (vertexList t)
    where
        variableMapForNode :: Node (v a) -> M.Map a [Node (v a)]
        variableMapForNode n = foldr (\var acc -> M.insert var [n] acc) M.empty (label n.v)

treeWidth :: (ValuationFamily v, Var a) => JoinTree (v a) -> Natural
treeWidth = fromIntegral . maximum . map (S.size . (.d)) . vertexList

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
outgoingGraphEdges :: Id -> G.AdjacencyMap (Node v) -> Maybe (Node v, [Node v])
outgoingGraphEdges i g = L.find (\(n, _) -> n.id == i) . G.adjacencyList $ g

flipEdge :: Node a -> Node a -> G.AdjacencyMap (Node a) -> G.AdjacencyMap (Node a)
flipEdge x y g
    | G.hasEdge x y g = addEdge y x $ G.removeEdge x y $ g
    | otherwise = g

    where
        addEdge :: (Ord a) => a -> a -> G.AdjacencyMap a -> G.AdjacencyMap a
        addEdge x1 x2 g' = G.overlay (G.connect (G.vertex x1) (G.vertex x2)) g'

renumberTopological :: G.AdjacencyMap (Node a) -> G.AdjacencyMap (Node a)
renumberTopological g = G.gmap (\n -> changeId n $ (M.!) newNumbering n.id) g
    where
        topological = U.fromRight $ G.topSort $ G.toAdjacencyMap g
        newNumbering = M.fromList $ zip (map (.id) topological) [0..]

        changeId n newId = n { id = newId }

isConnected :: (Ord a) => G.AdjacencyMap a -> Bool
isConnected g = case G.vertexList g of
    []       -> True
    xs@(x:_) -> length (G.reachable undirected x) == length xs
    where
        undirected = G.overlay g (G.transpose g)


-- | Returns the edges of a given tree such that the arrows point *up* the tree as follows:
--
--                                  1
--                                  ▲
--                                  │
--                              ┌───│───┐
--                              │   │   │
--                              4   2   3
--
edgesOfTree :: T.Tree a -> [(a, a)]
edgesOfTree (T.Node _    [])       = []
edgesOfTree (T.Node root subTrees) = map (\s -> (s, root)) subTreeRoots  -- The edges from subtrees to root
                                      ++ concatMap edgesOfTree subTrees  -- plus the edges of all subtrees
    where
        subTreeRoots = map (.rootLabel) subTrees


--------------------------------------------------------------------------------
-- Invariants
--------------------------------------------------------------------------------
-- | Invariant checking the underlying graph forms a tree
-- (connected, acyclic graph, with at most |V| - 1 edges)
--
-- __Warning__: Doesn't check whether graph is connected, as this is checked
-- by another invariant anyway.
isTree :: JoinTree v -> Bool
isTree t = G.edgeCount t.g == G.vertexCount t.g - 1
                && isAcyclic t

isAcyclic :: JoinTree v -> Bool
isAcyclic t = isJust . G.toAcyclic . G.toAdjacencyMap $ t.g

hasRunningIntersectionProperty :: forall v a . (Valuation v a) => JoinTree (v a) -> Bool
hasRunningIntersectionProperty t = all (isConnected . inducedByVar) (M.keys variableMap')
    where
        variableMap' = variableMap t

        inducedByVar :: a -> G.AdjacencyMap (Node (v a))
        inducedByVar var = G.induce (\n -> n `elem` (M.!) variableMap' var) t.g

-- | Returns true if the given tree is directed towards the root node.
-- Assumes given tree is acyclic.
isConnectedAndDirectedTowardsRoot :: JoinTree v -> Bool
isConnectedAndDirectedTowardsRoot t = length canReachRoot == vertexCount t
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

        root = t.root

        vertexHasValidPostbox :: Node v -> Bool
        vertexHasValidPostbox n
            | Nothing <- n.postbox            = True
            | Just p  <- n.postbox, n == root = U.allButMaybeOne (`elem` neighbours n) (M.keys p)  -- [*]
            | Just p  <- n.postbox            = all              (`elem` neighbours n) (M.keys p)

        -- [*] Condition is slightly weakened to support an easier implementation
        -- of distribute for `MessagePassing.Threads` - the algorithm used there
        -- suffers from the fact that the root node of a subtree may contain a message
        -- from its child in the real tree.

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



--------------------------------------------------------------------------------
-- Tree Width Analysis
--------------------------------------------------------------------------------
{-# NOINLINE maxTreeWidthTracker #-}
maxTreeWidthTracker :: IO.IORef Natural
maxTreeWidthTracker = IO.unsafePerformIO (IO.newIORef 0)

-- TODO: NOT REDUNDANT CONSTRAINTS
{-# NOINLINE trackMaxTreeWidth #-}
trackMaxTreeWidth :: (V.ValuationFamily v, V.Var a) => JoinTree (v a) -> JoinTree (v a)
trackMaxTreeWidth t = IO.unsafePerformIO $ do
    currentMax <- U.getGlobal maxTreeWidthTracker
    U.setGlobal maxTreeWidthTracker (max (treeWidth t) currentMax)
    pure $ t

trackMaxTreeWidth' :: (ValuationFamily v, Var a) => v a -> v a
#if !defined(COUNT_OPERATIONS) || !(COUNT_OPERATIONS)
trackMaxTreeWidth' = id
#else
{-# NOINLINE trackMaxTreeWidth' #-}
trackMaxTreeWidth' v = IO.unsafePerformIO $ do
    currentMax <- U.getGlobal maxTreeWidthTracker
    U.setGlobal maxTreeWidthTracker (max (treeWidthOfV v) currentMax)
    pure $ v

    where
        treeWidthOfV = fromIntegral . length . label
#endif

{-# NOINLINE numUsedValuationsTracker #-}
numUsedValuationsTracker :: IO.IORef Int
numUsedValuationsTracker = IO.unsafePerformIO (IO.newIORef 0)

{-# NOINLINE trackUsedValuations #-}
trackUsedValuations :: JoinTree (v a) -> JoinTree (v a)
trackUsedValuations t = IO.unsafePerformIO $ do
    currentMax <- U.getGlobal numUsedValuationsTracker
    U.setGlobal numUsedValuationsTracker (max numValuations currentMax)
    pure $ t

    where
        numValuations = length . filter (\n -> n.t == Valuation) . vertexList $ t

trackUsedValuations' :: [a] -> [a]
#if !defined(COUNT_OPERATIONS) || !(COUNT_OPERATIONS)
trackUsedValuations' = id
#else
{-# NOINLINE trackUsedValuations' #-}
trackUsedValuations' vs = IO.unsafePerformIO $ do
    currentMax <- U.getGlobal numUsedValuationsTracker
    U.setGlobal numUsedValuationsTracker (max (length vs) currentMax)
    pure $ vs
#endif

tracking :: (ValuationFamily v, Var a) => JoinTree (v a) -> JoinTree (v a)
#if !defined(COUNT_OPERATIONS) || !(COUNT_OPERATIONS)
tracking = id
#else
{-# NOINLINE tracking #-}
tracking = trackUsedValuations . trackMaxTreeWidth
#endif



