-- | A baseline implementation of djikstras algorithm imported
-- from hackage to test the localcomputation implementation against.
--
-- All functions assert cost self loops on the input graphs. This is
-- important as it is easy to forget that the results of using the
-- GenericInference algorithm assume that the shortest path of a node
-- to itself has a cost of 0, while this implementation doesn't.
module Benchmarks.ShortestDistance.Baseline (
      shortestPaths
    , singleSource
    , singleTarget

    -- Graph construction
    , fromGraph
    , toGraph
    , empty
    , merge
    , merges
    , merges1
) where

import           Control.Exception              (assert)
import qualified Data.Map                       as M
import qualified Graph.DijkstraSimple           as H
import qualified Graph.DijkstraSimple.Weighters as H
import qualified GenericInference.Graph         as G

shortestPaths :: (Ord a, Ord b, Num b) => H.Graph a b -> a -> M.Map a b
shortestPaths g _ | assert (hasZeroCostSelfLoops g) False = undefined
shortestPaths g source = M.map (\(H.Path _ cost) -> cost) paths
    where
        (H.Paths paths) = H.lightestPaths g source H.cumulativeWeighter

singleSource :: (Ord a, Ord b, Num b) => H.Graph a b -> a -> [a] -> b -> [b]
singleSource graph _ _ _ | assert (hasZeroCostSelfLoops graph) False = undefined
singleSource graph source targets unreachable = map (\t -> M.findWithDefault unreachable t shortest) targets
    where
        shortest = shortestPaths graph source

{- | Returns the single target shortest path solutions for a graph that has been broken up into smaller graphs.

__Warning__: This function should not be used for benhmarking purposes as it includes the overhead of switching
all arc directions of the given graphs.
-}
singleTarget :: (Ord a, Ord b, Num b) => [G.Graph a b] -> [a] -> a -> b -> [b]
singleTarget graphs _ _ _ | assert (all G.hasZeroCostSelfLoops graphs) False = undefined
singleTarget graphs sources target unreachable = singleSource graph target sources unreachable
    where
        graph = fromGraph $ G.merges1 $ map G.flipArcDirections graphs

--------------------------------------------------------------------------------
-- Graph construction
--------------------------------------------------------------------------------
fromGraph :: G.Graph a b -> H.Graph a b
fromGraph g = H.Graph $ M.map (map (\(destination, cost) -> H.EdgeTo destination cost)) (G.toMap g)

toGraph :: (Ord a) => H.Graph a b -> G.Graph a b
toGraph (H.Graph m) = G.unsafeFromMap $ M.map (map (\(H.EdgeTo destination cost) -> (destination, cost))) m

empty :: H.Graph a b
empty = H.Graph M.empty

merge :: (Ord a) => H.Graph a b -> H.Graph a b -> H.Graph a b
merge (H.Graph g1) (H.Graph g2) = H.Graph $ M.unionWith (++) g1 g2

merges :: (Foldable f, Ord a) => H.Graph a b -> f (H.Graph a b) -> H.Graph a b
merges initial gs = foldr merge initial gs

merges1 :: (Foldable f, Ord a) => f (H.Graph a b) -> H.Graph a b
merges1 gs = foldr1 merge gs

hasZeroCostSelfLoops :: (Ord a, Eq b, Num b) => H.Graph a b -> Bool
hasZeroCostSelfLoops = G.hasZeroCostSelfLoops . toGraph
