{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE OverloadedLists   #-}

module Main (main) where

import qualified Algebra.Graph                    as Directed
import           Algebra.Graph.Undirected
import           Control.Monad                    (when)
import           Data.Binary                      (Binary)
import           Data.Char                        (ord)
import           Data.List                        (intersperse)
import qualified Data.Map                         as M
import           Data.Time                        (getCurrentTime)
import           GHC.Generics

-- LocalComputation library files
import           Inference.Collect
import           Inference.ShenoyShafer
import           Instances.Bayesian
import           Inference.JoinTree
import           LocalProcess
import           Utils
import           ValuationAlgebra
import           ValuationAlgebra.Semiring


---- We will need these someday (probably)
import           Control.Concurrent               (threadDelay)
import           Control.Distributed.Process
import           Control.Distributed.Process.Node
import           Network.Transport.TCP

-- -- uses not-undirected graph so commented out for now
showAdjacents :: (Ord a, Show a) => (Directed.Graph a) -> String
showAdjacents graph = concat $ intersperse "\n" $ fmap (show) (Directed.adjacencyList graph)

showNodes :: (Ord a, Show a) => (Directed.Graph a) -> String
showNodes graph = concat $ intersperse "\n" $ fmap show (Directed.vertexList graph)


data MainParameters = MainParameters {
    printP1JoinTree           :: Bool,
    printP2JoinTree           :: Bool,
    performP1ShenoyInference  :: Bool,
    queryP1Network            :: Bool,
    performP2SeminarInference :: Bool,
    test                      :: Bool
}

main :: IO ()
main = runProcessLocal $ mainProcess (MainParameters {
    printP1JoinTree = False,
    printP2JoinTree = False,
    performP1ShenoyInference = False,
    queryP1Network = True,
    performP2SeminarInference = False,
    test = False
})

mainProcess :: MainParameters -> Process ()
mainProcess _ = do

    liftIO $ putStrLn "\n\n\n\n\nRunning Program...\n\n"
    liftIO $ putStrLn "\nFinished.\n"

    -- when (printP1JoinTree params) $
    --     liftIO $ print $ length $ showAdjacents $ p1BasicTree


-- p1BasicTree :: Directed.Graph (CollectNode (SemiringValuation Probability) P1Var P1Value)
-- p1BasicTree = baseJoinTree p1Valuations p1Queries
--
--
-- p2BasicTree :: Directed.Graph (CollectNode (SemiringValuation Probability) P2Var P2Value)
-- p2BasicTree = baseJoinTree p2Valuations p2Query

