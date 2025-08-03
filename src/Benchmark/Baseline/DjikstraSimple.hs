module Benchmark.Baseline.DjikstraSimple
    (
      create
    , shortestPaths
    , singleSource
    , singleTarget
    , empty
    , merge
    , merges
    , merges1
    )
where

import qualified Data.Map                       as M
import qualified Graph.DijkstraSimple           as G
import qualified Graph.DijkstraSimple.Weighters as G
import qualified LocalComputation.Graph         as LG

create :: LG.Graph a b -> G.Graph a b
create xs = G.Graph $ M.map (map (\(destination, cost) -> G.EdgeTo destination cost)) (LG.toMap xs)

empty :: G.Graph a b
empty = G.Graph M.empty

merge :: (Ord a) => G.Graph a b -> G.Graph a b -> G.Graph a b
merge (G.Graph g1) (G.Graph g2) = G.Graph $ M.unionWith (++) g1 g2

merges :: (Foldable f, Ord a) => G.Graph a b -> f (G.Graph a b) -> G.Graph a b
merges initial gs = foldr merge initial gs

merges1 :: (Foldable f, Ord a) => f (G.Graph a b) -> G.Graph a b
merges1 gs = foldr1 merge gs

shortestPaths :: (Ord a, Ord b, Num b) => G.Graph a b -> a -> M.Map a b
shortestPaths xs source = M.map (\(G.Path _ cost) -> cost) paths
    where
        (G.Paths paths) = G.lightestPaths xs source G.cumulativeWeighter

singleSource :: (Ord a, Ord b, Num b) => G.Graph a b -> a -> [a] -> b -> [b]
singleSource graph source targets unreachable = map (\t -> M.findWithDefault unreachable t shortest) targets
    where
        shortest = shortestPaths graph source

{- | Returns the single target shortest path solutions for a graph that has been broken up into smaller graphs.

__Warning__: This function should not be used for benhmarking purposes as it includes the overhead of switching
all arc directions of the given graphs.
-}
singleTarget :: (Ord a, Ord b, Num b) => [LG.Graph a b] -> [a] -> a -> b -> [b]
singleTarget graphs sources target unreachable = singleSource graph target sources unreachable
    where
        graph = create $ LG.merges1 $ map LG.flipArcDirections graphs

