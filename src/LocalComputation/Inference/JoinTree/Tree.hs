{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}

module LocalComputation.Inference.JoinTree.Tree
    (
    -- Join tree type
      JoinTree (g)
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
    )
where

import           GHC.Records                       (HasField, getField)
import           LocalComputation.ValuationAlgebra

import qualified Algebra.Graph                     as G
import qualified Algebra.Graph.ToGraph             as G (toAdjacencyMap,
                                                         topSort)
import qualified Algebra.Graph.Undirected          as UG
import qualified Data.Bifunctor                    as B
import qualified Data.List                         as L
import qualified Data.Map                          as M
import           Data.Maybe                        (fromJust)
import           Data.Text.Lazy                    (unpack)
import qualified LocalComputation.Utils            as U
import           Text.Pretty.Simple                (pShow)

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

-- TODO: Should have a seperate data structure for join trees. That way we
-- not only can better assert the tree is in a set structure, but we can
-- also potentially get better performance for operations such as
-- "find a node with *this* id".

-- TODO: A join tree is really a join forest. Once we create a proper data
-- structure for join trees with invariants, this will be more evident.

-- TODO: Add invaraint that it can't be empty to make `root` safe.
newtype JoinTree v = UnsafeJoinTree { g :: G.Graph (Node v) }

instance HasField "root" (JoinTree v) (Node v) where
    getField t = L.maximumBy (\x y -> x.id `compare` y.id) $ G.vertexList t.g

fromGraph :: G.Graph (Node v) -> JoinTree v
fromGraph = UnsafeJoinTree

find :: (Node v -> Bool) -> JoinTree v -> Maybe (Node v)
find p t = undefined

findById :: Id -> JoinTree v -> Maybe (Node v)
findById i t = L.find (\n -> n.id == i) $ G.vertexList t.g

transpose :: JoinTree v -> JoinTree v
transpose t = fromGraph $ G.transpose t.g

--------------------------------------------------------------------------------
-- Transformations
--------------------------------------------------------------------------------

-- | Flips an edge from x to y such that it now goes from y to x.
flipEdge :: Node a -> Node a -> JoinTree a -> JoinTree a
flipEdge x y t
    | G.hasEdge x y t.g = fromGraph $ addEdge y x $ G.removeEdge x y $ t.g
    | otherwise = t

    where
        addEdge :: a -> a -> G.Graph a -> G.Graph a
        addEdge x1 x2 g = G.overlay (G.connect (G.vertex x1) (G.vertex x2)) g

mapVertices :: (Node a -> Node b) -> JoinTree a -> JoinTree b
mapVertices f t = fromGraph $ fmap f t.g

--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------

vertexList :: JoinTree v -> [Node v]
vertexList t = G.vertexList t.g

neighbours :: Id -> JoinTree v -> [Node v]
neighbours = undefined

neighbourMap :: JoinTree v -> M.Map Id [Node v]
neighbourMap t = M.fromList . map (B.first (.id)) . UG.adjacencyList . UG.toUndirected $ t.g

incomingEdges :: Id -> JoinTree v -> Maybe [Node v]
incomingEdges = (fmap snd .) . incomingEdges'

incomingEdges' :: Id -> JoinTree v -> Maybe (Node v, [Node v])
incomingEdges' i t = outgoingEdges' i $ transpose t

outgoingEdges :: Id -> JoinTree v -> Maybe [Node v]
outgoingEdges = (fmap snd .) . outgoingEdges'

outgoingEdges' :: Id -> JoinTree v -> Maybe (Node v, [Node v])
outgoingEdges' i t = L.find (\(n, _) -> n.id == i) . G.adjacencyList $ t.g

topologicalOrdering :: JoinTree v -> [Node v]
topologicalOrdering t = U.fromRight $ G.topSort $ G.toAdjacencyMap t.g

--------------------------------------------------------------------------------
-- Unsafe variants
--------------------------------------------------------------------------------

unsafeFindById :: Id -> JoinTree v -> Node v
unsafeFindById = (fromJust . ) . findById

unsafeIncomingEdges :: Id -> JoinTree v -> [Node v]
unsafeIncomingEdges = (fromJust .) . incomingEdges

unsafeIncomingEdges' :: Id -> JoinTree v -> (Node v, [Node v])
unsafeIncomingEdges' = (fromJust .) . incomingEdges'

unsafeOutgoingEdges :: Id -> JoinTree v -> [Node v]
unsafeOutgoingEdges = (fromJust .) . outgoingEdges

unsafeOutgoingEdges' :: Id -> JoinTree v -> (Node v, [Node v])
unsafeOutgoingEdges' = (fromJust .) . outgoingEdges'






