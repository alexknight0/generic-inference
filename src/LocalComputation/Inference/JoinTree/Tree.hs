{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LocalComputation.Inference.JoinTree.Tree (
    -- Join tree type
    -- TODO: remove g?
      JoinForest (g)
    , Node (id, v, t)
    , node
    , changeContent
    , NodeType (Valuation, Query, Union, Projection)
    , Id

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
    , flipEdge
    , topologicalOrdering
    , isForest
    , supportsCollect
) where

import           GHC.Records                        (HasField, getField)
import           LocalComputation.ValuationAlgebra  hiding (assertInvariants,
                                                     satisfiesInvariants)

import qualified Algebra.Graph                      as G
import qualified Algebra.Graph.Acyclic.AdjacencyMap as G (toAcyclic)
import qualified Algebra.Graph.ToGraph              as G (dfsForest, reachable,
                                                          toAdjacencyMap,
                                                          topSort)
import qualified Algebra.Graph.Undirected           as UG
import           Control.Exception                  (assert)
import qualified Data.Bifunctor                     as B
import qualified Data.List                          as L
import qualified Data.Map                           as M
import           Data.Maybe                         (fromJust, isJust)
import qualified Data.Set                           as S
import           Data.Text.Lazy                     (unpack)
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

-- TODO: Maybe remove these.
instance Eq (Node v) where
    x == y = x.id == y.id

instance Ord (Node v) where
    x <= y = x.id <= y.id

instance (Valuation v, Ord a, Show a) => Show (Node (v a)) where
    show n = unpack $ pShow (n.id, n.d)

--------------------------------------------------------------------------------
-- Join Trees
--------------------------------------------------------------------------------

-- TODO: Invariants! For example we should know that this is a forest at every point in time.
-- (this implies everything is a tree!!!)

-- TODO: Add invaraint that it can't be empty to make `root` safe.
newtype JoinForest v = UnsafeJoinForest { g :: G.Graph (Node v) }

instance HasField "root" (JoinForest v) (Node v) where
    getField t = L.maximumBy (\x y -> x.id `compare` y.id) $ G.vertexList t.g

fromGraph :: G.Graph (Node v) -> JoinForest v
fromGraph = assertInvariants . UnsafeJoinForest

findById :: Id -> JoinForest v -> Maybe (Node v)
findById i t = L.find (\n -> n.id == i) $ G.vertexList t.g

transpose :: JoinForest v -> JoinForest v
transpose t = fromGraph $ G.transpose t.g

--------------------------------------------------------------------------------
-- Transformations
--------------------------------------------------------------------------------

-- | Flips an edge from x to y such that it now goes from y to x.
flipEdge :: Node a -> Node a -> JoinForest a -> JoinForest a
flipEdge x y t
    | G.hasEdge x y t.g = fromGraph $ addEdge y x $ G.removeEdge x y $ t.g
    | otherwise = t

    where
        addEdge :: a -> a -> G.Graph a -> G.Graph a
        addEdge x1 x2 g = G.overlay (G.connect (G.vertex x1) (G.vertex x2)) g

mapVertices :: (Node a -> Node b) -> JoinForest a -> JoinForest b
mapVertices f t = fromGraph $ fmap f t.g

--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------

-- TODO: Should also check that join tree is directed towards the query node
-- (in the relevant join tree amongst the forest)
supportsCollect :: JoinForest v -> Bool
supportsCollect t = numQueryNodes t == 1 && isQueryNodeRoot t

-- TODO: If is DAG and is not forest than should just be equal to whether
-- or not all vertices can reach the root.
isDirectedTowardsRoot :: JoinForest v -> Bool
isDirectedTowardsRoot t = undefined

isQueryNodeRoot :: JoinForest v -> Bool
isQueryNodeRoot t = t.root.t == Query

numQueryNodes :: JoinForest v -> Natural
numQueryNodes t = L.genericLength $ filter (\n -> n.t == Query) $ vertexList t

-- A join tree is a DAG with the property that
-- every edge is directed towards the root node
-- (i.e. all vertices can reach the root node)
--
-- It also has the running intersection property...
-- but let's not test that here. It should hold since
-- we only really create trees through one construction algorithm,
-- and we never modify these trees by adding or removing an edge.
isForest :: JoinForest v -> Bool
isForest t = length (splitForest t) > 1

splitForest :: forall v . JoinForest v -> [JoinForest v]
splitForest t = getTrees' (vertexSet t)

    where
        undirected = G.overlay (t.g) (G.transpose t.g)

        getTrees' :: S.Set (Node v) -> [JoinForest v]
        getTrees' vertices
            | length vertices == 0 = []
            | otherwise            = newTree : getTrees' (S.difference vertices verticesInNewTree)

            where
                -- Take a random vertex out
                vertexInNewTree = S.findMin vertices

                -- Take out all the vertices in its tree
                verticesInNewTree = S.fromList $ G.reachable undirected vertexInNewTree

                -- Get the tree
                newTree = fromGraph $ G.induce (\n -> n `elem` verticesInNewTree) t.g


vertexList :: JoinForest v -> [Node v]
vertexList t = G.vertexList t.g

vertexSet :: JoinForest v -> S.Set (Node v)
vertexSet t = G.vertexSet t.g

neighbourMap :: JoinForest v -> M.Map Id [Node v]
neighbourMap t = M.fromList . map (B.first (.id)) . UG.adjacencyList . UG.toUndirected $ t.g

incomingEdges :: Id -> JoinForest v -> Maybe [Node v]
incomingEdges = (fmap snd .) . incomingEdges'

incomingEdges' :: Id -> JoinForest v -> Maybe (Node v, [Node v])
incomingEdges' i t = outgoingEdges' i $ transpose t

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




--------------------------------------------------------------------------------
-- Invariants
--------------------------------------------------------------------------------

isAcyclic :: JoinForest v -> Bool
isAcyclic t = isJust . G.toAcyclic . G.toAdjacencyMap $ t.g

-- TODO: is acyclic
satisfiesInvariants :: JoinForest v -> Bool
satisfiesInvariants t = isAcyclic t

assertInvariants :: JoinForest v -> JoinForest v
assertInvariants t = assert (satisfiesInvariants t) t



