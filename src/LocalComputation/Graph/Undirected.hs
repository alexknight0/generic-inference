{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

-- | Module with a data type for an undirected, weightless graph with no self loops or double edges.
-- Made to be imported qualified.
module LocalComputation.Graph.Undirected (
      Graph

    -- Construction
    , unsafeFromList
    , fromGraph
    , fromEdgeList
    , toAlgebraGraph
    , fromAlgebraGraph

    -- Properties
    , isEmpty
    , isComplete
    , ascVertexList
    , vertexSet
    , edgeSet
    , unsafeHead

    -- Transformations
    , unsafeNeighbours
    , induce
    , removeVertex
    , addEdges
) where
import qualified Algebra.Graph.Undirected as UG
import           Control.DeepSeq          (NFData)
import qualified Data.Bifunctor           as B
import qualified Data.Map                 as M
import qualified Data.Set                 as S
import qualified Data.Text.Lazy           as LT
import           Data.Tuple               (swap)
import           GHC.Generics             (Generic)
import qualified LocalComputation.Graph   as G
import qualified LocalComputation.Utils   as U
import           Prelude                  hiding (all)
import qualified Prelude                  as P
import           Text.Pretty.Simple       (pShowNoColor)

-- TODO: We could try an adjacency matrix representation and see if that changes the complexity.

-- | A undirected, weightless graph with no self loops or double edges.
newtype Graph a = Graph { m :: M.Map a (S.Set a) } deriving (Generic, NFData, Eq)

instance (Show a) => Show (Graph a) where
    show (Graph g) = LT.unpack $ pShowNoColor g

--------------------------------------------------------------------------------
-- Construction
--------------------------------------------------------------------------------
unsafeFromMap :: (Ord a) => M.Map a (S.Set a) -> Graph a
unsafeFromMap = U.assertP satisfiesInvariants . Graph

unsafeFromList :: (Ord a) => [(a, [a])] -> Graph a
unsafeFromList xs = unsafeFromMap $ M.fromList $ map (B.second S.fromList) xs

-- | Safely constructs a graph from the given adjacency list.
--
-- If the graph has self loops, they will be discarded.
-- Directed edges become undirected edges.
fromList :: (Ord a) => [(a, [a])] -> Graph a
fromList xs = fromEdgeList edges
    where
        vertices = map fst xs
        edges = edgeListFromAdjacencyList xs
                    ++ map (\v -> (v, v)) vertices   -- Vertices only present in self loops
                                                     -- will transform into vertices with no edges.

-- | Safely constructs a graph from the given edge list.
--
-- If the graph has self loops, they will be discarded.
-- Directed edges become undirected edges.
-- Obviously, any vertices not present in an edge will not be added.
-- However, if a self loop exists containing that vertex, the vertex
-- will still be added.
fromEdgeList :: (Ord a) => [(a, a)] -> Graph a
fromEdgeList edges = unsafeFromMap $ M.mapWithKey (\x adj -> S.delete x adj) -- delete self loops
                                   $ M.fromListWith S.union
                                   $ undirectedEdges

    where
        undirectedEdges = map (B.second S.singleton) edges
                       ++ map (B.second S.singleton) (map swap edges)

fromGraph :: (Ord a) => G.Graph a b -> Graph a
fromGraph g = fromEdgeList $ map (\e -> (e.arcHead, e.arcTail)) $ G.toList g

toAlgebraGraph :: Graph a -> UG.Graph a
toAlgebraGraph = UG.stars . adjacencyList

fromAlgebraGraph :: (Ord a) => UG.Graph a -> Graph a
fromAlgebraGraph = fromList . UG.adjacencyList

--------------------------------------------------------------------------------
-- Invariants
--------------------------------------------------------------------------------
-- | The graph:
--  2. Has no self loops
--  1. Is undirected
satisfiesInvariants :: (Ord a) => Graph a -> Bool
satisfiesInvariants g = P.all (\x -> not $ S.member (x, x) edgeSet') (ascVertexList g)    -- (2)
                     && P.all (\(x, y) -> S.member (y, x) edgeSet') edges                 -- (1)
    where

        edges = ascEdgeList g
        edgeSet' = S.fromList edges

--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------
isEmpty :: Graph a -> Bool
isEmpty g = M.null g.m

ascVertexList :: Graph a -> [a]
ascVertexList g = M.keys g.m

vertexSet :: Graph a -> S.Set a
vertexSet g = M.keysSet g.m

-- | Returns the edge list of a given graph
--
-- __Warning__: If there are any vertices that are not present
-- in any edges, they will not be returned in this list!!
ascEdgeList :: Graph a -> [(a, a)]
ascEdgeList g = edgeListFromAdjacencyList $ adjacencyList g

edgeSet :: (Ord a) => Graph a -> S.Set (a, a)
edgeSet g = S.fromList (ascEdgeList g)

-- | Returns the adjacency list associated with the graph.
adjacencyList :: Graph a -> [(a, [a])]
adjacencyList g = map (B.second S.toList) $ M.toAscList $ g.m

isComplete :: (Ord a) => Graph a -> Bool
isComplete g
    | otherwise = all (\v neighbours -> neighbours == S.delete v allVertices) g
    where
        allVertices = vertexSet g

all :: (a -> S.Set a -> Bool) -> Graph a -> Bool
all p g = M.foldrWithKey f True g.m
    where
        f key value acc = p key value && acc

unsafeHead :: Graph a -> a
unsafeHead g = fst $ M.findMin g.m

--------------------------------------------------------------------------------
-- Transformations
--------------------------------------------------------------------------------

induce :: (Ord a) => S.Set a -> Graph a -> Graph a
induce retained g = unsafeFromMap $ M.map (\adjs -> S.intersection adjs retained)   -- Remove from values
                                  $ M.restrictKeys g.m retained                     -- Remove from keys

removeVertex :: (Ord a) => a -> Graph a -> Graph a
removeVertex x g = unsafeFromMap $ M.map (\adjs -> S.delete x adjs)     -- Remove from values
                                 $ M.delete x g.m                       -- Remove from keys

unsafeNeighbours :: (Ord a) => a -> Graph a -> S.Set a
unsafeNeighbours x g = (M.!) g.m x

addVertex :: (Ord a) => a -> Graph a -> Graph a
addVertex x g = unsafeFromMap $ M.insertWith (\y _ -> y) x S.empty g.m

addEdge :: (Ord a) => (a, a) -> Graph a -> Graph a
addEdge (x, y) g
    | x == y    = addVertex x g  -- We aren't adding the edge, but if the vertex isn't there we should add it.
    | otherwise = unsafeFromMap $ M.insertWith S.union y (S.singleton x)        -- Add the revered edge
                                $ M.insertWith S.union x (S.singleton y) g.m    -- Add the edge

addEdges :: (Ord a) => [(a, a)] -> Graph a -> Graph a
addEdges edges g = foldr addEdge g edges -- union g (fromEdgeList edges)

union :: (Ord a) => Graph a -> Graph a -> Graph a
union g1 g2 = unsafeFromMap $ M.unionWith S.union g1.m g2.m

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------
edgeListFromAdjacencyList :: [(a, [a])] -> [(a, a)]
edgeListFromAdjacencyList = concatMap (\(v, adjs) -> [(v, adj) | adj <- adjs])


--------------------------------------------------------------------------------
-- Example
--------------------------------------------------------------------------------

_exampleGraph :: Graph Int
_exampleGraph = unsafeFromList [ (1, [2, 3])
                                  , (2, [1, 4])
                                  , (3, [1])
                                  , (4, [2])
                                ]



