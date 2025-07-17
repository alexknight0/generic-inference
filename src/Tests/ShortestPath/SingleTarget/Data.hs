module Tests.ShortestPath.SingleTarget.Data
    ( p1Graph
    , p1Queries
    , p1Answers
    , p2Graph
    )
where

import           Benchmark.Baseline.DjikstraSimple                            (DistanceGraph)
import qualified Data.Map                                                     as M
import qualified LocalComputation.Instances.ShortestPath.Parser               as P
import qualified LocalComputation.Instances.ShortestPath.SingleTarget         as S (Graph)
import           LocalComputation.Utils                                       (assert',
                                                                               parseFile)
import           LocalComputation.ValuationAlgebra.QuasiRegular.SemiringValue
import           Numeric.Natural                                              (Natural)
import qualified Text.Parsec                                                  as P (ParseError)

{- | Example graph used for a shortest path problem.

Source: https://www.geeksforgeeks.org/dsa/dijkstras-algorithm-for-adjacency-list-representation-greedy-algo-8/
-}
p1AndP2Basis :: (Num a) => [(Integer, [(Integer, a)])]
p1AndP2Basis = [
            (0, [(0, 0), (1, 4), (7, 8)])
          , (1, [(1, 0), (0, 4), (2, 8), (7, 11)])
          , (2, [(2, 0), (1, 8), (3, 7), (5, 4), (8, 2)])
          , (3, [(3, 0), (2, 7), (4, 9), (5, 14)])
          , (4, [(4, 0), (3, 9), (5, 10)])
          , (5, [(5, 0), (2, 4), (3, 14), (4, 10), (6, 2)])
          , (6, [(6, 0), (5, 2), (7, 1), (8, 6)])
          , (7, [(7, 0), (0, 8), (1, 11), (6, 1), (8, 7)])
          , (8, [(8, 0), (2, 2), (6, 6), (7, 7)])
      ]

p1Graph :: (Num a) => S.Graph Integer a
p1Graph = M.fromList p1AndP2Basis

-- P1 but with the information split across 8 individual graphs that must be combined together.
p2Graph' :: (Num a) => [S.Graph Integer a]
p2Graph' = map (M.fromList . (:[])) p1AndP2Basis

p1Queries :: ([Integer], Integer)
p1Queries = ([ 1
             , 2
             , 3
             , 4
             , 5
             , 6
             , 7
             , 8
            ], 0)

p1Answers :: [TropicalSemiringValue]
p1Answers = [ 4
            , 12
            , 19
            , 21
            , 11
            , 9
            , 8
            , 14
           ]

p2Graph = p3Graph

p3Graph :: IO (Either P.ParseError (Either P.InvalidGraphFile (P.Graph Natural Integer)))
p3Graph = parseFile P.graph "src/Benchmark/Data/ShortestPath/ParseTestSmall-USA-road-d.NY.gr"

isValidGraph :: (Eq a, Eq b) => DistanceGraph a b -> Bool
isValidGraph xs = all (\((x, y), c) -> ((y, x), c) `elem` xs') xs'
    where
        xs' = concat xs

