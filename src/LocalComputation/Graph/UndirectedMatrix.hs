{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

-- | Module with a data type for an undirected, weightless graph with no self loops or double edges.
-- Made to be imported qualified.
module LocalComputation.Graph.UndirectedMatrix (
      Graph

    -- Construction
    , fromGraph
    , fromList
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
    , unsafeRemoveVertex
    , addEdges
) where
import qualified Algebra.Graph.Undirected        as UG
import           Control.DeepSeq                 (NFData)
import qualified Data.Bifunctor                  as B
import qualified Data.Map                        as M
import qualified Data.Set                        as S
import qualified Data.Text.Lazy                  as LT
import           Data.Tuple                      (swap)
import           GHC.Generics                    (Generic)
import           GHC.Stack                       (HasCallStack)
import qualified LocalComputation.Graph          as G
import qualified LocalComputation.LabelledMatrix as L
import qualified LocalComputation.Utils          as U
import           Prelude                         hiding (all)
import qualified Prelude                         as P
import           Text.Pretty.Simple              (pShowNoColor)

-- TODO: We could try an adjacency matrix representation and see if that changes the complexity.

-- | A undirected, weightless graph with no self loops or double edges.
newtype Graph a = Graph { m :: L.LabelledMatrix a a Bool } deriving (Generic, NFData, Eq)

instance (Show a) => Show (Graph a) where
    show (Graph g) = LT.unpack $ pShowNoColor g

--------------------------------------------------------------------------------
-- Construction
--------------------------------------------------------------------------------
unsafeFromMatrix :: (HasCallStack, Ord a) => L.LabelledMatrix a a Bool -> Graph a
unsafeFromMatrix = U.assertP satisfiesInvariants . Graph

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

-- TODO: If we used an unboxed labelled matrix that does not
-- have the condition we imposed that the rows columns must be in
-- that order we might have better peformance...

-- | Safely constructs a graph from the given edge list.
--
-- If the graph has self loops, they will be discarded.
-- Directed edges become undirected edges.
-- Obviously, any vertices not present in an edge will not be added.
-- However, if a self loop exists containing that vertex, the vertex
-- will still be added.
fromEdgeList :: (Ord a) => [(a, a)] -> Graph a
fromEdgeList edges = unsafeFromMatrix $ L.mapWithKey deleteSelfLoops  -- Remove self loops
                                      $ L.fromListDefault False
                                      $ undirectedEdges

    where
        undirectedEdges = concatMap (\x -> [(x, True), (swap x, True)]) edges

        deleteSelfLoops row col old
            | row == col = False
            | otherwise  = old

fromGraph :: (Ord a) => G.Graph a b -> Graph a
fromGraph g = fromEdgeList $ map (\e -> (e.arcHead, e.arcTail)) $ G.toEdgeList g

toAlgebraGraph :: Graph a -> UG.Graph a
toAlgebraGraph = undefined -- UG.stars . adjacencyList

fromAlgebraGraph :: (Ord a) => UG.Graph a -> Graph a
fromAlgebraGraph = undefined -- fromList . UG.adjacencyList

--------------------------------------------------------------------------------
-- Invariants
--------------------------------------------------------------------------------
-- | The following invariants hold. When we say '(x, y)' we mean 'position (x, y) in the matrix'
--  1. (x, y) is valid for all x, y in vertices   (valid matrix)
--  2. If (x, y) == True then (y, x)              (undirected graph)
--  3. (x, x) == False for all x                  (no self loops)

-- need to check rowlabels equals collabels
satisfiesInvariants :: (Ord a) => Graph a -> Bool
satisfiesInvariants g = g.m.rowLabelSet == g.m.colLabelSet
                            && L.all' (\x y value -> x /= y || value == False) g.m
                            && L.all' (\x y value -> if value == True
                                                        then L.unsafeLookup y x g.m == True
                                                        else L.unsafeLookup y x g.m == False) g.m

--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------
isEmpty :: (Eq a) => Graph a -> Bool
isEmpty g = L.domain g.m == (S.empty, S.empty)

ascVertexList :: Graph a -> [a]
ascVertexList g = g.m.rowLabelList

vertexSet :: Graph a -> S.Set a
vertexSet g = g.m.rowLabelSet

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
adjacencyList g = zip vertices neighbours
    where
        neighbours = map (U.boolMask vertices) $ L.rowList g.m

        vertices = ascVertexList g

isComplete :: (Ord a) => Graph a -> Bool
isComplete g
    | otherwise = P.all (U.one (== False)) (L.rowList g.m)
    where
        allVertices = vertexSet g

unsafeHead :: Graph a -> a
unsafeHead g = head $ g.m.rowLabelList

--------------------------------------------------------------------------------
-- Transformations
--------------------------------------------------------------------------------
induce :: (Ord a) => S.Set a -> Graph a -> Graph a
induce retained g = unsafeFromMatrix $ L.unsafeProject g.m retained retained

unsafeRemoveVertex :: (Ord a) => a -> Graph a -> Graph a
unsafeRemoveVertex x g = unsafeFromMatrix $ L.unsafeProject g.m (S.delete x g.m.rowLabelSet) (S.delete x g.m.colLabelSet)

unsafeNeighbours :: (Ord a) => a -> Graph a -> S.Set a
unsafeNeighbours x g = S.fromList $ U.boolMask g.m.rowLabelList (L.unsafeRow x g.m)

addVertex :: (Ord a) => a -> Graph a -> Graph a
addVertex x g = undefined

addEdge :: (Ord a) => (a, a) -> Graph a -> Graph a
addEdge (x, y) g
    | x == y    = addVertex x g  -- We aren't adding the edge, but if the vertex isn't there we should add it.
    | otherwise = unsafeFromMatrix $ L.update y x True      -- Add the reverse edge
                                   $ L.update x y True g.m  -- Add the edge

-- TODO: Doesn't add new edges if the vertices don't exist.
addEdges :: (Ord a) => [(a, a)] -> Graph a -> Graph a
addEdges edges g = unsafeFromMatrix $ L.updates undirectedEdges g.m
    where
        undirectedEdges = concatMap (\x -> [(x, True), (swap x, True)]) edges

union :: (Ord a) => Graph a -> Graph a -> Graph a
union g1 g2 = undefined

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------
edgeListFromAdjacencyList :: [(a, [a])] -> [(a, a)]
edgeListFromAdjacencyList = concatMap (\(v, adjs) -> [(v, adj) | adj <- adjs])


--------------------------------------------------------------------------------
-- Example
--------------------------------------------------------------------------------

-- _exampleGraph :: Graph Int
-- _exampleGraph = unsafeFromList [ (1, [2, 3])
--                                   , (2, [1, 4])
--                                   , (3, [1])
--                                   , (4, [2])
--                                 ]



