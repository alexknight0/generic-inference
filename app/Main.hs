{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main (main) where

import Algebra.Graph
import Data.Binary
import Data.List (intersperse, nub, union)
import Data.Typeable
import Debug.Trace

-- LocalComputation library files
import ValuationAlgebra
import Collect
import Bayesian


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


main :: IO ()
main = do
    -- putStrLn $ showAdjacents p1JoinTree
    -- print $ map label p1Valuations
    print "foo"

-- p1JoinTree :: Graph (CollectNode (BVal P1Value) P1Var)
-- p1JoinTree = joinTree p1Valuations
