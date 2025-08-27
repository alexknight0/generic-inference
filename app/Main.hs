{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE OverloadedLists   #-}

module Main (main) where

import qualified Algebra.Graph                                                as Directed
import           Algebra.Graph.Undirected
import           Control.Monad                                                (when)
import           Data.Binary                                                  (Binary)
import           Data.Char                                                    (ord)
import           Data.List                                                    (intersperse)
import qualified Data.Map                                                     as M
import           Data.Time                                                    (getCurrentTime)
import           GHC.Generics

-- LocalComputation library files
import           LocalComputation.Inference.Collect
import           LocalComputation.Inference.JoinTree
import qualified LocalComputation.Inference.JoinTree.Diagram                  as D
import           LocalComputation.Inference.ShenoyShafer
import           LocalComputation.LocalProcess
import           LocalComputation.Utils
import           LocalComputation.ValuationAlgebra
import           LocalComputation.ValuationAlgebra.Semiring
import           Tests.BayesianNetwork.Data


---- We will need these someday (probably)
import           Control.Concurrent                                           (threadDelay)
import           Control.Distributed.Process
import           Control.Distributed.Process.Node
import           LocalComputation.Instances.BayesianNetwork
import qualified LocalComputation.Instances.ShortestPath.SingleTarget         as ST
import           LocalComputation.ValuationAlgebra.QuasiRegular.SemiringValue (TropicalSemiringValue (..))
import           Network.Transport.TCP
import           Tests.BayesianNetwork                                        (dataToValuations)
import           Text.Pretty.Simple                                           (pPrint)

import           Diagrams.Backend.SVG.CmdLine                                 (mainWith)

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
main = pure () -- mainWith (D.tree p1BasicTree)

-- main :: IO ()
-- main = runProcessLocal $ mainProcess (MainParameters {
--     printP1JoinTree = False,
--     printP2JoinTree = False,
--     performP1ShenoyInference = False,
--     queryP1Network = True,
--     performP2SeminarInference = False,
--     test = False
-- })

mainProcess :: MainParameters -> Process ()
mainProcess _ = do

    liftIO $ putStrLn "\n\n\n\n\nRunning Program...\n\n"
    liftIO $ putStrLn "[Did Nothing]"
    -- p3Small <- liftIO $ D.p3SmallGraph'
    -- results <- liftIO $ runProcessLocal $ ST.singleTarget [fmap T p3Small] [68] 69
    -- case results of
    --     Left ST.MissingZeroCostSelfLoops -> undefined
    --     Right xs                         -> pPrint xs


    liftIO $ putStrLn "\nFinished.\n"

    -- when (printP1JoinTree params) $
    --     liftIO $ print $ length $ showAdjacents $ p1BasicTree


-- p1BasicTree :: Directed.Graph (Node ((SemiringValuation Probability) AsiaVar Bool))
-- p1BasicTree = baseJoinTree (dataToValuations asiaValuationsP1) asiaQueriesP1
--
--
-- p2BasicTree :: Directed.Graph (CollectNode (SemiringValuation Probability) P2Var P2Value)
-- p2BasicTree = baseJoinTree p2Valuations p2Query

