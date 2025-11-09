module GenericInference.Inference.Triangulation (
      triangulate
    , maximalCliques
) where
import qualified Algebra.Graph.Undirected          as UG
import qualified Control.Exception                 as E (assert)
import qualified Data.List                         as L
import           Data.Maybe                        (fromJust)
import qualified Data.Set                          as S
import qualified GenericInference.Graph.Undirected as G
import qualified GenericInference.Utils            as U
import           Prelude                           hiding (cycle)

--------------------------------------------------------------------------------
-- Settings
--------------------------------------------------------------------------------
-- Asserts on this are not implemented efficently and hence really expensive for large graphs.
-- This is occasionally toggled on for testing; if a more efficient algorithm for checking
-- triangulation is written it can be left on!
enableAsserts :: Bool
enableAsserts = False

--------------------------------------------------------------------------------
-- Maximal Cliques
--------------------------------------------------------------------------------
-- | Returns the maximal cliques of a graph.
--
-- Some of the cliques in the returned list may not be maximal, but all
-- maximal cliques will be present.
--
-- Inspired by: https://www.cs.uoi.gr/~stavros/MSc-Algorithmic-GT/Triangulated.pdf
maximalCliques :: (Ord a) => G.Graph a -> [S.Set a]
maximalCliques g = maximalCliques' $ triangulate g

-- | Returns the maximal cliques as calculated on a triangulated graph.
--
-- __Warning__: Assumes the given graph is triangulated.
--
-- Inspired by: https://www.cs.uoi.gr/~stavros/MSc-Algorithmic-GT/Triangulated.pdf
maximalCliques' :: (Ord a) => G.Graph a -> [S.Set a]
maximalCliques' g | assert' (isTriangulated g) False = undefined
maximalCliques' g
    | G.isEmpty g = []
    | otherwise   = simplicialClique : maximalCliques' (G.removeVertex simplicial g)

    where
        simplicial = findSimplicial g

        simplicialClique = S.insert simplicial (G.unsafeNeighbours simplicial g)

findSimplicial :: (Ord a) => G.Graph a -> a
findSimplicial g = fromJust $ L.find (\v -> isSimplicial v g) $ G.ascVertexList g

-- | Variant of `isSimplicial` that takes the adjacency list as a parameter.
isSimplicial :: (Ord a) => a -> G.Graph a -> Bool
isSimplicial x g = G.isComplete graphOfNeighbourhood
    where
        neighbourhood = S.insert x (G.unsafeNeighbours x g)

        graphOfNeighbourhood = G.induce neighbourhood g

--------------------------------------------------------------------------------
-- Triangulation
--------------------------------------------------------------------------------
triangulate :: (Ord a) => G.Graph a -> G.Graph a
triangulate g = assertP' isTriangulated $ G.addEdges (triangulate' g []) g

triangulate' :: Ord a => G.Graph a -> [(a, a)] -> [(a, a)]
triangulate' g newEdges
    | G.isEmpty g = newEdges
    | otherwise = triangulate' graphAfterElimination (newEdges ++ addedEdges)

    where
        (graphAfterElimination, addedEdges) = eliminateNode g

-- Eliminates a node from the given graph, connecting all pairs of nodes that were neighbours
-- to the eliminated node. Returns a tuple of the new graph after elimination, and the edges
-- that were added to the new graph as a result of the elimination.
eliminateNode :: (Ord a) => G.Graph a -> (G.Graph a, [(a, a)])
eliminateNode g | assert' (not . G.isEmpty $ g) False = undefined  -- Can't eliminate a node when no nodes.
eliminateNode g = (G.addEdges newEdges (G.removeVertex toEliminate g), newEdges)
    where
        toEliminate = G.unsafeHead g
        newEdges = map U.toTuple $ U.combinations 2 $ S.toList $ G.unsafeNeighbours toEliminate g

--------------------------------------------------------------------------------
-- Triangulation detection (currently inefficient - used for assertions)
--------------------------------------------------------------------------------
isTriangulated :: (Ord a) => G.Graph a -> Bool
isTriangulated = isTriangulated' . G.toAlgebraGraph

isTriangulated' :: (Ord a) => UG.Graph a -> Bool
isTriangulated' g = all (\c -> cycleHasChord c g) cyclesWithMoreThanFourVertices
    where

        cycles = concat $ map (\v -> cyclesFromVertex v g) $ UG.vertexList g

        -- Since the first and last vertex are the same, four unique vertices in a cycle
        -- corresponds to a cycle of length 5.
        cyclesWithMoreThanFourVertices = filter ((>= 5) . length) cycles

cycleHasChord :: (Ord a) => [a] -> UG.Graph a -> Bool
cycleHasChord cycle _ | assert' (head cycle == last cycle) False = undefined
cycleHasChord cycle g = any (\(x, y) -> UG.hasEdge x y g) nonAdjacentPairs
    where
        -- Contains all possible pairs of elements from the cycle that are adjacent to each other
        -- in the cycle in both directions.
        adjacents = adjacentsOfCycle cycle

        nonAdjacentPairs = filter (\(x, y) -> not $ (x, y) `S.member` adjacents)
                                $ filter (\(x, y) -> not $ x == y)  -- self loops don't count as a non-adjacent pair
                                $ map U.toTuple
                                $ U.combinations 2 cycle

adjacentsOfCycle :: (Ord a) => [a] -> S.Set (a, a)
adjacentsOfCycle (x : y : rest) = S.insert (x, y) $ S.insert (y, x) $ adjacentsOfCycle (y : rest)
adjacentsOfCycle _              = S.empty


cyclesFromVertex :: (Ord a) => a -> UG.Graph a -> [[a]]
cyclesFromVertex source g = concat $ map pathsFromNeighbour $ neighboursNoSelfLoops source g
    where
        pathsFromNeighbour n = map (source:) $ pathsWithNoRepeatVertices n source (UG.removeEdge source n g)


pathsWithNoRepeatVertices :: Ord a => a -> a -> UG.Graph a -> [[a]]
pathsWithNoRepeatVertices source destination g
    | source == destination = [[destination]]
    | otherwise = concat $ map pathsFromNeighbour (neighboursNoSelfLoops source g)
    where
        pathsFromNeighbour n = map (source:) $ pathsWithNoRepeatVertices n destination graphWithoutSource

        graphWithoutSource = UG.removeVertex source g

neighboursNoSelfLoops :: (Ord a) => a -> UG.Graph a -> [a]
neighboursNoSelfLoops x = S.toList . S.filter (/= x) . UG.neighbours x

--------------------------------------------------------------------------------
-- Asserts
--------------------------------------------------------------------------------
assert' :: Bool -> a -> a
assert' = case enableAsserts of
                True  -> E.assert
                False -> (\_ y -> y)

assertP' :: (a -> Bool) -> a -> a
assertP' = case enableAsserts of
                True  -> U.assertP
                False -> (\_ y -> y)

--------------------------------------------------------------------------------
-- Example
--------------------------------------------------------------------------------

_exampleGraph :: G.Graph String
_exampleGraph = G.fromEdgeList [("A", "T")
                              , ("T", "L")
                              , ("T", "E")
                              , ("L", "E")
                              , ("L", "S")
                              , ("E", "D")
                              , ("E", "X")
                              , ("E", "B")
                              , ("D", "B")
                              , ("S", "B")
                            ]

{-
>> maximalCliques _exampleGraph
[fromList ["A","T"],
 fromList ["B","D","E","S"],
 fromList ["D","E","S"],
 fromList ["E","L","S","T","X"],
 fromList ["L","S","T","X"],
 fromList ["S","T","X"],
 fromList ["T","X"],
 fromList ["X"]
]

>> triangulate $ G.toAlgebraGraph $ _exampleGraph
edges [("A","T"),
       ("B","D"),
       ("B","E"),
       ("B","S"),
       ("D","E"),
       ("D","S"),
       ("E","L"),
       ("E","S"),
       ("E","T"),
       ("E","X"),
       ("L","S"),
       ("L","T"),
       ("L","X"),
       ("S","T"),
       ("S","X"),
       ("T","X")]
-}


