module Benchmark.Baseline.DjikstraSimple
    (
      create
    , shortestPaths
    , singleSource
    , singleTarget
    )
where

import qualified Data.Map                       as M
import qualified Graph.DijkstraSimple           as G
import qualified Graph.DijkstraSimple.Weighters as G
import qualified LocalComputation.Graph         as LG

create :: LG.Graph a b -> G.Graph a b
create xs = G.Graph $ M.map (map (\(destination, cost) -> G.EdgeTo destination cost)) (LG.toMap xs)

shortestPaths :: (Ord a, Ord b, Num b) => G.Graph a b -> a -> M.Map a b
shortestPaths xs source = M.map (\(G.Path _ cost) -> cost) paths
    where
        (G.Paths paths) = G.lightestPaths xs source G.cumulativeWeighter

singleSource :: (Ord a, Ord b, Num b) => G.Graph a b -> a -> [a] -> b -> [b]
singleSource graph source targets infinity = map (\t -> M.findWithDefault infinity t shortest) targets
    where
        shortest = shortestPaths graph source
-- singleSource graph source targets = map (\t -> shortest M.! t) targets
--     where
--         shortest = shortestPaths graph source

singleTarget :: (Ord a, Ord b, Num b) => LG.Graph a b -> [a] -> a -> b -> [b]
singleTarget graph sources target infinity = singleSource (create $ LG.flipArcDirections graph) target sources infinity

