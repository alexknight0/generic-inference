module Tests.ShortestPath.SingleTarget.Data
    ( p1Graph
    , p1Queries
    , p1Answers
    )
where

import           Benchmark.Baseline.DjikstraSimple                            (DistanceGraph)
import           LocalComputation.Utils                                       (assert')
import           LocalComputation.ValuationAlgebra.QuasiRegular.SemiringValue

{- | Example graph used for a shortest path problem.

Source: https://www.geeksforgeeks.org/dsa/dijkstras-algorithm-for-adjacency-list-representation-greedy-algo-8/
-}
p1Graph :: (Eq a, Num a) => DistanceGraph Integer a
p1Graph = assert' isValidGraph [
            [((0, 0), 0), ((0, 1), 4), ((0, 7), 8)]
          , [((1, 1), 0), ((1, 0), 4), ((1, 2), 8), ((1, 7), 11)]
          , [((2, 2), 0), ((2, 1), 8), ((2, 3), 7), ((2, 5), 4), ((2, 8), 2)]
          , [((3, 3), 0), ((3, 2), 7), ((3, 4), 9), ((3, 5), 14)]
          , [((4, 4), 0), ((4, 3), 9), ((4, 5), 10)]
          , [((5, 5), 0), ((5, 2), 4), ((5, 3), 14), ((5, 4), 10), ((5, 6), 2)]
          , [((6, 6), 0), ((6, 5), 2), ((6, 7), 1), ((6, 8), 6)]
          , [((7, 7), 0), ((7, 0), 8), ((7, 1), 11), ((7, 6), 1), ((7, 8), 7)]
          , [((8, 8), 0), ((8, 2), 2), ((8, 6), 6), ((8, 7), 7)]
        ]

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



isValidGraph :: (Eq a, Eq b) => DistanceGraph a b -> Bool
isValidGraph xs = all (\((x, y), c) -> ((y, x), c) `elem` xs') xs'
    where
        xs' = concat xs
