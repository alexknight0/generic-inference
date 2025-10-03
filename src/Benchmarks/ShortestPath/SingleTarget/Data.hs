{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

{- | Tests for benchmarking suite. Also exposes some example problems that can be used in a test suite -}
module Benchmarks.ShortestPath.SingleTarget.Data (
      Problem (..)
    , GraphAndQuery (..)
    , BenchmarkProblem (..)
    , genQuery
    , genConnectedQuery
    , genConnectedQueries
    , genGraph
    , genGraphs
    , p0Graphs
    , p0Queries
    , p1
    , p2
    , p3VerySmallGraph
    , p3SmallGraph
    , p3SmallGraph'
    , p3MediumGraph
    , p3MediumGraph'
    , p3VeryLargeGraph
    , createRandomProblem
) where

import qualified Benchmarks.Utils                                     as U
import qualified LocalComputation.Graph                               as G
import qualified LocalComputation.Instances.ShortestPath.Parser       as P
import           LocalComputation.Utils                               (fromRight,
                                                                       parseFile)
import           Numeric.Natural                                      (Natural)
import qualified Text.Parsec                                          as P (ParseError)

import qualified Data.List.Extra                                      as L
import qualified Data.Set                                             as S
import           Hedgehog
import qualified Hedgehog.Gen                                         as Gen
import qualified Hedgehog.Range                                       as Range
import           LocalComputation.Instances.ShortestPath.SingleTarget (Query (..))
import qualified LocalComputation.ValuationAlgebra                    as V

import           Control.Monad.IO.Class                               (MonadIO (liftIO))

-- | Problem definition
data Problem = Problem {
      graphs  :: [G.Graph Integer Double]
    , q       :: Query Integer
    , answers :: [Double]
}

-- | A problem where solutions are not specified - only used for benchmarking
-- Can specify multiple queries.
data GraphAndQuery a = GraphAndQuery {
      g  :: G.Graph a Double
    , qs :: [Query a]
} deriving (V.Generic, V.NFData)

data BenchmarkProblem a = BenchmarkProblem {
      name        :: String
    , numProblems :: Natural
    , numQueries  :: Natural
    , numVertices :: Natural
    , numEdges    :: Natural
    , edgeRatio   :: Double
    , ps          :: [GraphAndQuery a]
} deriving (V.Generic, V.NFData)

createRandomProblem :: (MonadIO m) => Natural -> Natural -> Natural -> Double -> Int -> m (BenchmarkProblem Natural)
createRandomProblem numProblems numQueries numVertices edgeRatio seed = do
    problems <- liftIO $ U.sample seed $ genGraphsAndQueries numProblems numQueries numVertices numEdges

    pure $ BenchmarkProblem "Randomly Generated" numProblems numQueries numVertices numEdges edgeRatio problems

    where
        numEdges = negativeBecomesZero $ floor $ fromIntegral numVertices * edgeRatio

        negativeBecomesZero = max 0

dataDirectory :: FilePath
dataDirectory = "src/Benchmarks/ShortestPath/SingleTarget/Data/"

--------------------------------------------------------------------------------
-- Random test generation
--------------------------------------------------------------------------------

-- | Generates a random query from the given set of graph vertices.
genQuery :: (Ord a) => S.Set a -> Gen (Query a)
genQuery vertices
    | null vertices = error "Expected non-empty vertices iterable"
    | otherwise = do
    target <- Gen.element vertices
    sources <- Gen.set (Range.linear 1 (length vertices - 1)) (Gen.element vertices)
    pure $ Query (S.toList sources) target

-- | Generates a random connected query given a reverse adjacency list.
genConnectedQuery :: [(a, [a])] -> Gen (Query a)
genConnectedQuery reverseAdjacencyList = do
    (target, possibleSources) <- Gen.element reverseAdjacencyList
    sources <- Gen.subsequence possibleSources
    pure $ Query sources target

-- | Generates a random list of queries given a graph and a number of queries to generate.
genConnectedQueries :: (Ord a) => Natural -> G.Graph a b -> Gen ([Query a])
genConnectedQueries numQueries g = Gen.list (Range.singleton $ fromIntegral numQueries)
                                            (genConnectedQuery reverseAdjacencyList)
    where
        reverseAdjacencyList = G.reverseAdjacencyList g

genEdge :: Gen a -> Gen b -> Gen (G.Edge a b)
genEdge genNode genCost = do
    arcHead <- genNode
    arcTail <- genNode
    cost <- genCost
    pure (G.Edge arcHead arcTail cost)

-- | Generates a random graph with the given number of nodes and edges.
-- The nodes are numbered from 0 to numNodes - 1. Edge costs range from 0 to 100.
-- Graph may be disconnected. After generation of the graph, one 0 cost self loop
-- is added to every node. Unaffected by size parameter so suitable for use with `Gen.sample`.
genGraph :: Natural -> Natural -> Gen (G.Graph Natural Double)
genGraph 0     _    = pure G.empty
genGraph nodes arcs = do
    edges <- Gen.list (Range.singleton $ fromIntegral arcs)
                      (genEdge genNode genCost)

    pure $ G.fromList (edges ++ selfLoops)

    where
        genNode = fmap fromIntegral $ Gen.int (Range.constant 0 (fromIntegral nodes - 1))
        genCost = fmap fromIntegral $ Gen.int (Range.constant 0 100)

        -- This approach of adding self loops also ensures that each node is actually
        -- present in the graph.
        selfLoops = [G.Edge x x 0 | x <- [0 .. nodes - 1]]

-- | Generates a bunch of graphs that together build a larger graph.
genGraphs :: Natural -> Natural -> Gen [G.Graph Natural Double]
genGraphs nodes arcs = do
    original <- genGraph nodes arcs

    pure $ map G.fromList
         $ L.chunksOf (max 1 $ div (fromIntegral nodes) 5)
         $ G.toList original

genQueryOnGraph :: (Ord a) => G.Graph a Double -> Natural -> Gen (GraphAndQuery a)
genQueryOnGraph g numQueries = do
    qs <- genConnectedQueries numQueries g

    pure $ GraphAndQuery g qs

genGraphAndQuery :: Natural -> Natural -> Natural -> Gen (GraphAndQuery Natural)
genGraphAndQuery numQueries nodes edges = do
    g <- genGraph nodes edges
    genQueryOnGraph g numQueries

genGraphsAndQueries :: Natural -> Natural -> Natural -> Natural -> Gen [GraphAndQuery Natural]
genGraphsAndQueries numProblems numQueries nodes edges = Gen.list (Range.singleton (fromIntegral numProblems))
                                                                  (genGraphAndQuery numQueries nodes edges)
--------------------------------------------------------------------------------
-- Manual tests
--------------------------------------------------------------------------------

-- | A collection of graphs inference should fail on due to missing a 0 cost self loop.
-- See `LocalComputation.Instances.ShortestPath.SingleTarget.hs` for more information.
p0Graphs :: (Num a) => [G.Graph Integer a]
p0Graphs = [
              G.fromList' [
                    (0, [(0, 0), (1, 4), (7, 8)])
                  , (1, [(1, 1), (0, 4), (2, 8), (7, 11)])
                  , (2, [(2, 0), (1, 8), (3, 7), (5, 4), (8, 2)])
                  , (3, [(3, 0), (2, 7), (4, 9), (5, 14)])
                  , (4, [(4, 0), (3, 9), (5, 10)])
                  , (5, [(5, 0), (2, 4), (3, 14), (4, 10), (6, 2)])
                  , (6, [(6, 0), (5, 2), (7, 1), (8, 6)])
                  , (7, [(7, 0), (0, 8), (1, 11), (6, 1), (8, 7)])
                  , (8, [(8, 0), (2, 2), (6, 6), (7, 7)])
              ]

            , G.fromList' [
                    (0, [(0, 0), (1, 4), (7, 8)])
                  , (1, [(1, 0), (0, 4), (2, 8), (7, 11)])
                  , (2, [        (1, 8), (3, 7), (5, 4), (8, 2)])
                  , (3, [(3, 0), (2, 7), (4, 9), (5, 14)])
                  , (4, [(4, 0), (3, 9), (5, 10)])
                  , (5, [(5, 0), (2, 4), (3, 14), (4, 10), (6, 2)])
                  , (6, [(6, 0), (5, 2), (7, 1), (8, 6)])
                  , (7, [(7, 0), (0, 8), (1, 11), (6, 1), (8, 7)])
                  , (8, [(8, 0), (2, 2), (6, 6), (7, 7)])
              ]
    ]

-- Random query. Shouldn't matter as probably won't be evaluated.
p0Queries :: Query Integer
p0Queries = Query [8] 7

{- | Example graph used for a shortest path problem.

Source: https://www.geeksforgeeks.org/dsa/dijkstras-algorithm-for-adjacency-list-representation-greedy-algo-8/
-}
p1AndP2Basis :: (Num a) => [(Integer, [(Integer, a)])]
p1AndP2Basis = [
            (0, [(1, 4), (7, 8)])
          , (1, [(0, 4), (2, 8), (7, 11)])
          , (2, [(1, 8), (3, 7), (5, 4), (8, 2)])
          , (3, [(2, 7), (4, 9), (5, 14)])
          , (4, [(3, 9), (5, 10)])
          , (5, [(2, 4), (3, 14), (4, 10), (6, 2)])
          , (6, [(5, 2), (7, 1), (8, 6)])
          , (7, [(0, 8), (1, 11), (6, 1), (8, 7)])
          , (8, [(2, 2), (6, 6), (7, 7)])
      ]

p1 :: Problem
p1 = Problem {
     -- Our fixed query answers assume 0 cost self loops
      graphs = [G.addSelfLoops 0 $ G.fromList' p1AndP2Basis]
    , q = Query [ 0
                , 1
                , 2
                , 3
                , 4
                , 5
                , 6
                , 7
                , 8
               ] 0
    , answers = [ 0
                , 4
                , 12
                , 19
                , 21
                , 11
                , 9
                , 8
                , 14
               ]
}

-- P1 but with the information split across 8 individual graphs that must be combined together.
p2 :: Problem
p2 = Problem {
      -- Our fixed query answers assume 0 cost self loops
      graphs = map (G.addSelfLoops 0 . G.fromList' . (:[])) p1AndP2Basis
    , q = Query [ 0
                , 5
                , 6
                , 7
                , 8
               ] 0
    , answers = [ 0
                , 11
                , 9
                , 8
                , 14
               ]
}

-------------------------------------------------------------------------------
-- Utils
-------------------------------------------------------------------------------

-- TODO: Don't think we need the self loops here? That should be handled by the wrapper
-- function...?
parseGraph :: FilePath -> IO (Either P.ParseError (Either P.InvalidGraphFile (G.Graph Natural Double)))
parseGraph filepath = fmap (P.mapParseResult (fromInteger)) $ parseFile P.graph filepath

parseGraph' :: FilePath -> IO (G.Graph Natural Double)
parseGraph' filepath = fmap (fromRight . fromRight) $ parseGraph filepath

p3VerySmallGraph :: IO (Either P.ParseError (Either P.InvalidGraphFile (G.Graph Natural Double)))
p3VerySmallGraph = parseGraph $ dataDirectory ++ "VerySmall-USA-road-d.NY.gr"

p3SmallGraph :: IO (Either P.ParseError (Either P.InvalidGraphFile (G.Graph Natural Double)))
p3SmallGraph = parseGraph $ dataDirectory ++ "Small-USA-road-d.NY.gr"

p3SmallGraph' :: IO (G.Graph Natural Double)
p3SmallGraph' = P.fromValid $ p3SmallGraph

p3MediumGraph :: IO (Either P.ParseError (Either P.InvalidGraphFile (G.Graph Natural Double)))
p3MediumGraph = parseGraph $ dataDirectory ++ "Medium-USA-road-d.NY.gr"

p3MediumGraph' :: IO (G.Graph Natural Double)
p3MediumGraph' = P.fromValid $ p3MediumGraph

p3VeryLargeGraph :: IO (Either P.ParseError (Either P.InvalidGraphFile (G.Graph Natural Double)))
p3VeryLargeGraph = parseGraph $ dataDirectory ++ "VeryLarge-USA-road-d.NY.gr"

