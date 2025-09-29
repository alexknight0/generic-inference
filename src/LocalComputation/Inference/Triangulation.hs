module LocalComputation.Inference.Triangulation (
      triangulate
    , maximalCliques
) where
import qualified Algebra.Graph.Undirected as UG
import           Control.Exception        (assert)
import qualified Data.List                as L
import           Data.Maybe               (fromJust)
import qualified Data.Set                 as S
import qualified LocalComputation.Utils   as U
import           Prelude                  hiding (cycle)

--------------------------------------------------------------------------------
-- Maximal Cliques
--------------------------------------------------------------------------------
-- | Returns the maximal cliques of a graph.
--
-- Some of the cliques in the returned list may not be maximal, but all
-- maximal cliques will be present.
--
-- Inspired by: https://www.cs.uoi.gr/~stavros/MSc-Algorithmic-GT/Triangulated.pdf
maximalCliques :: (Ord a) => UG.Graph a -> [S.Set a]
maximalCliques g = maximalCliques' $ triangulate g

-- | Returns the maximal cliques as calculated on a triangulated graph.
--
-- __Warning__: Assumes the given graph is triangulated.
--
-- Inspired by: https://www.cs.uoi.gr/~stavros/MSc-Algorithmic-GT/Triangulated.pdf
maximalCliques' :: (Ord a) => UG.Graph a -> [S.Set a]
maximalCliques' g | assert (isTriangulated g) False = undefined
maximalCliques' g
    | UG.isEmpty g = []
    | otherwise    = simplicialClique : maximalCliques' (UG.removeVertex simplicial g)

    where
        simplicial = findSimplicial g

        simplicialClique = S.insert simplicial (UG.neighbours simplicial g)

findSimplicial :: (Ord a) => UG.Graph a -> a
findSimplicial g = fromJust $ L.find (\v -> isSimplicial v g) $ UG.vertexList g

isSimplicial :: (Ord a) => a -> UG.Graph a -> Bool
isSimplicial x g = isComplete graphOfNeighbourhood
    where
        neighbourhood = S.insert x (UG.neighbours x g)

        graphOfNeighbourhood = UG.induce (`S.member` neighbourhood) g

isComplete :: (Ord a) => UG.Graph a -> Bool
isComplete g = all (\v -> S.delete v (UG.neighbours v g) == S.delete v allVertices) allVertices
    where
        allVertices = UG.vertexSet g

--------------------------------------------------------------------------------
-- Triangulation
--------------------------------------------------------------------------------
triangulate :: (Ord a) => UG.Graph a -> UG.Graph a
triangulate g = U.assertP isTriangulated $ UG.overlay g (UG.edges $ triangulate' g [])

triangulate' :: Ord a => UG.Graph a -> [(a, a)] -> [(a, a)]
triangulate' g newEdges
    | UG.isEmpty g = newEdges
    | otherwise = triangulate' graphAfterElimination (newEdges ++ addedEdges)

    where
        (graphAfterElimination, addedEdges) = eliminateNode g

-- Eliminates a node from the given graph, connecting all pairs of nodes that were neighbours
-- to the eliminated node. Returns a tuple of the new graph after elimination, and the edges
-- that were added to the new graph as a result of the elimination.
eliminateNode :: (Ord a) => UG.Graph a -> (UG.Graph a, [(a, a)])
eliminateNode g | assert (not . UG.isEmpty $ g) False = undefined  -- Can't eliminate a node when no nodes.
eliminateNode g = (UG.overlay (UG.edges newEdges) (UG.removeVertex toEliminate g), newEdges)
    where
        toEliminate = head $ UG.vertexList g
        newEdges = map U.toTuple $ U.combinations 2 $ S.toList $ UG.neighbours toEliminate g

--------------------------------------------------------------------------------
-- Triangulation detection
--------------------------------------------------------------------------------
isTriangulated :: (Ord a) => UG.Graph a -> Bool
isTriangulated g = all (\c -> cycleHasChord c g) cyclesWithMoreThanFourVertices
    where

        cycles = concat $ map (\v -> cyclesFromVertex v g) $ UG.vertexList g

        -- Since the first and last vertex are the same, four unique vertices in a cycle
        -- corresponds to a cycle of length 5.
        cyclesWithMoreThanFourVertices = filter ((>= 5) . length) cycles

cycleHasChord :: (Ord a) => [a] -> UG.Graph a -> Bool
cycleHasChord cycle _ | assert (head cycle == last cycle) False = undefined
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

_exampleGraph :: UG.Graph String
_exampleGraph = UG.edges [("A", "T")
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
>>> maximalCliques (triangulate _exampleGraph)
[fromList ["A","T"],
 fromList ["B","D","E","S"],
 fromList ["D","E","S"],
 fromList ["E","L","S","T","X"],
 fromList ["L","S","T","X"],
 fromList ["S","T","X"],
 fromList ["T","X"],
 fromList ["X"]
 ]

>> triangulate $ _exampleGraph
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

