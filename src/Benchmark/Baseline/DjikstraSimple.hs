module Benchmark.Baseline.DjikstraSimple
    ( DistanceGraph
    , create
    , shortestPaths
    , answerQueries
    )
where

import qualified Data.Map                       as M
import qualified Graph.DijkstraSimple           as G
import qualified Graph.DijkstraSimple.Weighters as G

type DistanceGraph a b = [[((a, a), b)]]

create :: (Ord a) => DistanceGraph a b -> G.Graph a b
create xs = G.Graph $ foldr f (M.empty) (concat xs)
            where
                f ((origin, destination), cost) acc = M.insertWith (++) origin [G.EdgeTo destination cost] acc

shortestPaths :: (Ord a, Ord b, Num b) => G.Graph a b -> a -> M.Map a b
shortestPaths xs source = M.map (\(G.Path _ cost) -> cost) paths
    where
        (G.Paths paths) = G.lightestPaths xs source G.cumulativeWeighter

answerQueries :: (Ord a, Ord b, Num b) => G.Graph a b -> a -> [a] -> [b]
answerQueries graph source qs = map (\q -> shortest M.! q) qs
    where
        shortest = shortestPaths graph source
