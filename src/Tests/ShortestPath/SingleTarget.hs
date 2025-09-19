{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- | Tests the valuation algebra solving shortest path problems.
-- Also doubles up to test that all methods of inference produce
-- the same results - all tests verify this property.
module Tests.ShortestPath.SingleTarget
    ( tests )
where

import           Benchmarks.ShortestPath.SingleTarget                 as ST
import           Benchmarks.ShortestPath.SingleTarget.Data
import qualified LocalComputation.Graph                               as G
import qualified LocalComputation.Instances.ShortestPath.SingleTarget as ST (Query)

import           Hedgehog                                             hiding
                                                                      (test)
import qualified Hedgehog.Gen                                         as Gen
import qualified Hedgehog.Internal.Property                           as Hedgehog (PropertyName (..))
import qualified Hedgehog.Range                                       as Range

import           Control.Distributed.Process                          (liftIO)
import           Control.Monad                                        (forM)
import qualified LocalComputation.Inference.JoinTree.Diagram          as D
import qualified LocalComputation.Instances.ShortestPath.Parser       as P
import qualified Text.Parsec                                          as P

-- Typeclasses
import           Control.DeepSeq                                      (NFData)
import           Control.Monad.IO.Class                               (MonadIO)
import           Data.Binary                                          (Binary)
import qualified Data.Hashable                                        as H
import qualified LocalComputation.Inference                           as I
import qualified LocalComputation.Inference.MessagePassing            as MP
import           Numeric.Natural                                      (Natural)
import           Tests.Utils                                          (checkAnswers,
                                                                       unitTest)
import           Type.Reflection                                      (Typeable)

group :: GroupName
group = "Tests.ShorestPath.SingleTarget"

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------
tests :: IO Bool
tests = fmap and $ sequence [
        checkParallel $$(discover)
      , checkMatchesBaselineP3
      , checkMatchesBaselineRandom
   ]

checkMatchesBaselineP3 :: IO Bool
checkMatchesBaselineP3 = checkMatchesBaseline name getGraph test 100
    where
        name mode = "prop_matchesBaseline_onP3 " ++ show mode
        getGraph = P.fromValid p3VerySmallGraph
        test = matchesBaselineOnGraph

checkMatchesBaselineRandom :: IO Bool
checkMatchesBaselineRandom = checkMatchesBaseline name unused test 100
    where
        name mode = "prop_matchesBaseline_onRandom " ++ show mode
        unused = pure G.empty
        test _ = matchesBaselineOnRandomGraph

prop_p1_drawGraph :: Property
prop_p1_drawGraph = unitTest $ do
    results <- ST.singleTarget' (ST.Generic $ I.Shenoy MP.Threads) settings (head p1.graphs) p1.q
    checkAnswers approx results p1.answers

    where
        settings = D.def { D.beforeInference = Just "diagrams/p1_before.svg"
                         , D.afterInference  = Just "diagrams/p1_after.svg"
                        }

prop_p2_drawGraph :: Property
prop_p2_drawGraph = unitTest $ do
    results <- ST.singleTargetSplit' (ST.Generic $ I.Shenoy MP.Threads) settings p2.graphs p2.q
    checkAnswers approx results p2.answers
    where
        settings = D.def { D.beforeInference = Just "diagrams/p2_before.svg"
                         , D.afterInference  = Just "diagrams/p2_after.svg"
                        }

-- | Tests that the all implementations work for a set problem where one graph is given.
prop_p1 :: Property
prop_p1 = pX p1

-- | Tests that the all implementations work for a set problem where multiple graphs are given.
prop_p2 :: Property
prop_p2 = pX p2

-- | Tests the parser doesn't fail on a known working example.
prop_parser :: Property
prop_parser = unitTest $ do
    _ <- parseGraph p3SmallGraph
    parseGraph p3MediumGraph

--------------------------------------------------------------------------------
-- Settings
--------------------------------------------------------------------------------
tolerableError :: Double
tolerableError = 0.00000001

approx :: Double -> Double -> Bool
approx x y
    | abs (x - y) < tolerableError = True
    | x == (read "Infinity") && y == (read "Infinity") = True
    | x == (read "-Infinity") && y == (read "-Infinity") = True
    | otherwise = False


--------------------------------------------------------------------------------
-- Utilities for checking answers match baseline
--------------------------------------------------------------------------------
matchesBaselineOnRandomGraph :: Implementation -> TestLimit -> Property
matchesBaselineOnRandomGraph mode numTests = withTests numTests . property $ do
    nodes <- forAll $ Gen.int (Range.linear 1 100)   -- There is no valid query for 0 nodes.
    edges <- forAll $ Gen.int (Range.linear 0 1000)
    graphs <- forAll $ genGraphs (fromIntegral nodes) (fromIntegral edges)

    matchesBaselineForRandomQuery graphs mode

-- | Checks the output of the local computation algorithms and the baseline algorithm match for a set of random queries.
matchesBaselineOnGraph :: forall a . (NFData a, H.Hashable a, Binary a, Typeable a, Show a, Ord a)
    => G.Graph a Double
    -> Implementation
    -> TestLimit
    -> Property
matchesBaselineOnGraph g mode numTests = withTests numTests . property $ matchesBaselineForRandomQuery [g] mode

matchesBaselineForRandomQuery :: forall a . (NFData a, H.Hashable a, Binary a, Typeable a, Show a, Ord a)
    => [G.Graph a Double]
    -> Implementation
    -> PropertyT IO ()
matchesBaselineForRandomQuery gs mode = do
    query <- forAll $ genConnectedQuery reverseAdjacencyList

    baseline     <- go        Baseline query
    local        <- go        mode query
    localUnsplit <- goUnsplit mode query

    checkAnswers approx local        baseline
    checkAnswers approx localUnsplit baseline

    where
        go :: (MonadIO m) => Implementation -> ST.Query a -> m [Double]
        go m query = ST.singleTargetSplit' m D.def gs query

        goUnsplit :: (MonadIO m) => Implementation -> ST.Query a -> m [Double]
        goUnsplit m query = ST.singleTarget' m D.def fullGraph query

        fullGraph = G.merges1 gs

        reverseAdjacencyList = G.reverseAdjacencyList fullGraph


--------------------------------------------------------------------------------
-- Utilities for manual checks
--------------------------------------------------------------------------------
pX :: Problem -> Property
pX p = unitTest $ do
    forM ST.allImplementations $ \mode -> do
        results <- ST.singleTargetSplit' mode D.def p.graphs p.q
        checkAnswers approx (results) p.answers

-- | Parses the given graph. Fails if a parse error occurs.
parseGraph :: IO (Either P.ParseError (Either P.InvalidGraphFile a)) -> PropertyT IO a
parseGraph g = do
    parsed <- liftIO g
    case parsed of
        Left e  -> do annotateShow e; failure
        Right parseResult -> case parseResult of
            Left e  -> do annotateShow e; failure
            Right x -> pure x

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------
checkMatchesBaseline :: (ST.Implementation -> String)
                   -> IO (G.Graph Natural Double)
                   -> (G.Graph Natural Double -> ST.Implementation -> TestLimit -> Property)
                   -> TestLimit
                   -> IO Bool
checkMatchesBaseline name getGraph test numTests = do
    graph <- getGraph
    checkParallel $ Group group $ map (toProperty graph) ST.allButBaseline

    where
        toProperty :: G.Graph Natural Double -> Implementation -> (PropertyName, Property)
        toProperty g mode = (Hedgehog.PropertyName $ name mode, test g mode numTests)


