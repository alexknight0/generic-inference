{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}

module Tests.ShortestPath.SingleTarget.Data (
      Query (..)
    , p0Graphs
    , p0Queries
    , p1Graph
    , p1Queries
    , p1Answers
    , p2Graph
    , p2Queries
    , p2Answers
    , p3Graph
    , p4Graph
) where

import qualified LocalComputation.Graph                                       as G
import qualified LocalComputation.Instances.ShortestPath.Parser               as P
import           LocalComputation.Utils                                       (parseFile)
import           LocalComputation.ValuationAlgebra.QuasiRegular.SemiringValue
import           Numeric.Natural                                              (Natural)
import qualified Text.Parsec                                                  as P (ParseError)

data Query a = Query { sources :: [a], target :: a } deriving Show

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

p1Graph :: (Num a) => G.Graph Integer a
p1Graph = G.addSelfLoops 0 $ G.fromList' p1AndP2Basis

p1Queries :: Query Integer
p1Queries = Query [ 0
             , 1
             , 2
             , 3
             , 4
             , 5
             , 6
             , 7
             , 8
            ] 0

p1Answers :: [TropicalSemiringValue]
p1Answers = [ 0
            , 4
            , 12
            , 19
            , 21
            , 11
            , 9
            , 8
            , 14
           ]

-- P1 but with the information split across 8 individual graphs that must be combined together.
p2Graph :: (Num a) => [G.Graph Integer a]
p2Graph = map (G.addSelfLoops 0 . G.fromList' . (:[])) p1AndP2Basis

p2Queries :: Query Integer
p2Queries = p1Queries

p2Answers :: [TropicalSemiringValue]
p2Answers = p1Answers

parseGraph :: FilePath -> IO (Either P.ParseError (Either P.InvalidGraphFile (G.Graph Natural TropicalSemiringValue)))
parseGraph filepath = fmap (P.mapParseResult (T . fromInteger)) $ parseFile P.graph filepath

p3Graph :: IO (Either P.ParseError (Either P.InvalidGraphFile (G.Graph Natural TropicalSemiringValue)))
p3Graph = parseGraph "src/Benchmark/Data/ShortestPath/Small-USA-road-d.NY.gr"

p4Graph :: IO (Either P.ParseError (Either P.InvalidGraphFile (G.Graph Natural TropicalSemiringValue)))
p4Graph = parseGraph "src/Benchmark/Data/ShortestPath/USA-road-d.NY.gr"

