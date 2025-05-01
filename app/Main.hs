{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Algebra.Graph
import Data.List (intersperse)

-- LocalComputation library files
import ValuationAlgebra
import Collect
import Bayesian
import JoinTree


---- We will need these someday (probably)
-- import Control.Concurrent (threadDelay)
-- import Control.Distributed.Process
-- import Control.Distributed.Process.Node
-- import Network.Transport.TCP
-- import Data.Tree

----------
-- utils
------------

-- Consider creating a typeclass for variables that has them implement 'hashCode' - nenok uses
-- this to implement the binary heap ordering?

showAdjacents :: (Ord a, Show a) => (Graph a) -> String
showAdjacents graph = concat $ intersperse "\n\n\n" $ fmap (show) (adjacencyList graph)

data P1Var = F | B | L | D | H deriving (Eq, Ord, Show)

data P1Value = P1False | P1True deriving (Enum, Bounded, Show, Eq)

p1Valuations :: [BayesValuation P1Var P1Value]
p1Valuations =
    [ getRows $ Columns F [] [0.85, 0.15],
        getRows $ Columns B [] [0.99, 0.01],
        getRows $ Columns L [F] [0.95, 0.4, 0.05, 0.6],
        getRows $ Columns D [F, B] [0.7, 0.03, 0.1, 0.01, 0.3, 0.97, 0.9, 0.99],
        getRows $ Columns H [D] [0.99, 0.3, 0.01, 0.7]
    ]

p1JoinTree :: Graph (CollectNode BayesValuation P1Var P1Value)
p1JoinTree = joinTree p1Valuations

main :: IO ()
main = do
    -- putStrLn $ showAdjacents p1JoinTree
    -- print $ map label p1Valuations
    putStrLn $ showAdjacents p1JoinTree


