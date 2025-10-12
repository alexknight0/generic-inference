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
import           Benchmarks.ShortestPath.SingleTarget.Data            (Problem (answers, graphs, q),
                                                                       genConnectedQuery,
                                                                       genGraph,
                                                                       genGraphs,
                                                                       newYorkMedium,
                                                                       newYorkSmall,
                                                                       newYorkVerySmall,
                                                                       p1, p2)
import qualified LocalComputation.Graph                               as G
import qualified LocalComputation.Inference.Statistics                as S
import qualified LocalComputation.Instances.ShortestPath.SingleTarget as ST (Query,
                                                                             decomposition)

import           Hedgehog                                             hiding
                                                                      (assert,
                                                                       test)
import qualified Hedgehog.Gen                                         as Gen
import qualified Hedgehog.Internal.Property                           as Hedgehog (PropertyName (..))
import qualified Hedgehog.Range                                       as Range

import qualified Benchmarks.ShortestPath.SingleTarget.Data            as D
import           Control.Distributed.Process                          (liftIO)
import           Control.Monad                                        (forM)
import qualified LocalComputation.Inference.JoinTree.Diagram          as D

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

import           Data.IORef                                           (IORef,
                                                                       atomicModifyIORef',
                                                                       newIORef,
                                                                       readIORef)
import           System.IO.Unsafe                                     (unsafePerformIO)

group :: GroupName
group = "Tests.ShorestPath.SingleTarget"

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------
-- TODO: Test works if there are two valuations concerning the same variables but
-- with one having larger values for the distance between variables.

tests :: IO Bool
tests = fmap and $ sequence [
        checkParallel $$(discover)
      , checkMatchesBaselineNewYork
      , checkMatchesBaselineRandom
   ]

checkMatchesBaselineNewYork :: IO Bool
checkMatchesBaselineNewYork = checkMatchesBaseline name getGraph test 100
    where
        name mode = "prop_matchesBaseline_onP3 " ++ show mode
        getGraph = D.unsafeParseFullGraph newYorkVerySmall
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
    checkAnswers approx results.c p1.answers

    where
        settings = D.def { D.beforeInference = Just "diagrams/p1_before.svg"
                         , D.afterInference  = Just "diagrams/p1_after.svg"
                        }

prop_p2_drawGraph :: Property
prop_p2_drawGraph = unitTest $ do
    results <- ST.singleTargetSplit' (ST.Generic $ I.Shenoy MP.Threads) settings p2.graphs p2.q
    checkAnswers approx results.c p2.answers
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

-- | Tests the parser doesn't fail on known working examples.
prop_parser :: Property
prop_parser = unitTest $ do
    smallByExactParser <- parseFullGraph newYorkSmall
    mediumByExactParser <- parseFullGraph newYorkMedium

    smallByPartialParser <- parseGraph (edgeCount smallByExactParser) D.newYork
    mediumByPartialParser <- parseGraph (edgeCount mediumByExactParser) D.newYork

    smallByExactParser === smallByPartialParser
    mediumByExactParser === mediumByPartialParser

    where
        edgeCount = fromIntegral . G.edgeCount

prop_decomposition_keepsVertices :: Property
prop_decomposition_keepsVertices = withTests 100 . property $ do
    nodes <- forAll $ Gen.int (Range.linear 1 50)   -- There is no valid query for 0 nodes.
    edges <- forAll $ Gen.int (Range.linear 0 500)
    graph <- forAll $ genGraph (fromIntegral nodes) (fromIntegral edges)

    let decomposition = ST.decomposition graph

    (length decomposition >= 1) === True  -- Since nodes >= 1; check required as merges1 requires at least 1 elem.

    G.minimiseEdgeCosts (G.merges1 (ST.decomposition graph)) === G.minimiseEdgeCosts graph

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

globalGraphCounter :: IORef Int
{-# NOINLINE globalGraphCounter #-}
globalGraphCounter = unsafePerformIO (newIORef 1)

getGlobal :: IO Int
getGlobal = readIORef globalGraphCounter

incrementGlobal :: IO Int
incrementGlobal = atomicModifyIORef' globalGraphCounter (\x -> let x' = x + 1 in (x', x'))

matchesBaselineForRandomQuery :: forall a . (NFData a, H.Hashable a, Binary a, Typeable a, Show a, Ord a)
    => [G.Graph a Double]
    -> Implementation
    -> PropertyT IO ()
matchesBaselineForRandomQuery gs mode = do
    query <- forAll $ genConnectedQuery reverseAdjacencyList

    graphNum <- liftIO $ getGlobal
    _ <- liftIO $ incrementGlobal

    baseline     <- go        Baseline query
    local        <- go        mode query
    localUnsplit <- goUnsplit mode query graphNum

    checkAnswers approx local        baseline
    checkAnswers approx localUnsplit baseline

    where
        go :: (MonadIO m) => Implementation -> ST.Query a -> m [Double]
        go m query = fmap (.c) $ ST.singleTargetSplit' m D.def gs query

        goUnsplit :: (MonadIO m) => Implementation -> ST.Query a -> Int -> m [Double]
        goUnsplit m query graphNum = fmap (.c) $ ST.singleTarget' m s fullGraph query
            where
                -- | Toggle this test drawing graphs for debugging purposes.
                -- The parent function is used by multiple tests - only one test that uses
                -- the parent function should probably be active otherwise the drawn graphs
                -- will continuously get overridden, and it may crash if two tests try to
                -- write at the same time.
                drawGraph = False

                s = case drawGraph of
                        True  -> D.def { D.beforeInference = Just $ "diagrams/test/" ++ show graphNum ++ ".svg" }
                        False -> D.def

        fullGraph = G.merges1 gs

        reverseAdjacencyList = G.reverseAdjacencyList fullGraph

-- TODO: do we test empty queries?

--------------------------------------------------------------------------------
-- Utilities for manual checks
--------------------------------------------------------------------------------
pX :: Problem -> Property
pX p = unitTest $ do
    -- TODO: fix
    forM [DynamicP $ MP.Threads] $ \mode -> do
        results <- ST.singleTargetSplit' mode D.def p.graphs p.q
        checkAnswers approx (results.c) p.answers

-- | Parses the given graph. Fails if a parse error occurs.
parseFullGraph :: FilePath -> PropertyT IO (G.Graph Natural Double)
parseFullGraph filepath = do
    parsed <- liftIO $ D.parseFullGraph filepath
    case parsed of
        Left e  -> do annotateShow e; failure
        Right parseResult -> case parseResult of
            Left e  -> do annotateShow e; failure
            Right g -> pure g

-- | Parses a given number of lines of the given graph. Fails if a parse error occurs.
parseGraph :: Natural -> FilePath -> PropertyT IO (G.Graph Natural Double)
parseGraph numArcs filepath = do
    parsed <- liftIO $ D.parseGraph numArcs filepath
    case parsed of
        Left e  -> do annotateShow e; failure
        Right g -> pure g

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
    checkParallel $ Group group $ map (toProperty graph) (ST.allButBaseline)

    where
        toProperty :: G.Graph Natural Double -> Implementation -> (PropertyName, Property)
        toProperty g mode = (Hedgehog.PropertyName $ name mode, test g mode numTests)


