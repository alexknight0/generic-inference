{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- | Tests the valuation algebra solving shortest path problems.
-- Also doubles up to test that all methods of inference produce
-- the same results - all tests verify this property.
module Tests.ShortestPath.SingleTarget
    ( tests )
where

import qualified Benchmarks.ShortestPath.SingleTarget.Baseline        as H
import           Benchmarks.ShortestPath.SingleTarget.Data
import qualified LocalComputation.Graph                               as G
import qualified LocalComputation.Instances.ShortestPath.SingleTarget as ST
import           LocalComputation.Utils

import           Hedgehog
import qualified Hedgehog.Gen                                         as Gen
import qualified Hedgehog.Internal.Property                           as Hedgehog (PropertyName (..))
import qualified Hedgehog.Range                                       as Range

import           Control.Distributed.Process                          (Process,
                                                                       liftIO)
import           Control.Monad                                        (forM,
                                                                       forM_)
import qualified LocalComputation.Inference.JoinTree.Diagram          as D
import qualified LocalComputation.Instances.ShortestPath.Parser       as P
import qualified Text.Parsec                                          as P

-- Typeclasses
import           Control.DeepSeq                                      (NFData)
import           Control.Monad.IO.Class                               (MonadIO)
import           Data.Binary                                          (Binary)
import qualified Data.Hashable                                        as H
import           Debug.Trace                                          (traceShow)
import qualified LocalComputation.Inference                           as I
import           Numeric.Natural                                      (Natural)
import           Tests.Utils                                          (checkAnswers,
                                                                       unitTest)
import           Type.Reflection                                      (Typeable)

tests :: IO Bool
tests = fmap and $ sequence [
        checkParallel $$(discover)
      , p3MatchesBaseline
      , randomMatchesBaseline
   ]

p3MatchesBaseline :: IO Bool
p3MatchesBaseline = do
    p3VerySmall <- P.fromValid p3VerySmallGraph
    checkParallel $ Group "Tests.ShortestPath.SingleTarget" $ map (getTest p3VerySmall) [I.BruteForce, I.Fusion False, I.Shenoy]

    where
        getTest :: G.Graph Natural Double -> I.Mode -> (PropertyName, Property)
        getTest g mode = (Hedgehog.PropertyName $ "prop_p3VerySmall_MatchesBaseline_" ++ show mode
                         , matchesBaseline mode g 100)

-- TODO: Clean up
randomMatchesBaseline :: IO Bool
randomMatchesBaseline = do
    checkParallel $ Group "Tests.ShortestPath.SingleTarget" $ map getTest [I.BruteForce, I.Fusion False, I.Shenoy]

    where
        getTest :: I.Mode -> (PropertyName, Property)
        getTest mode = (Hedgehog.PropertyName $ "prop_random_MatchesBaseline_" ++ show mode
                      , randomMatchesBaseline' mode 100)

-- TODO: Something doesn't like empty graphs...
randomMatchesBaseline' :: I.Mode -> TestLimit -> Property
randomMatchesBaseline' mode numTests = withTests numTests . property $ do
    nodes <- forAll $ Gen.int (Range.linear 2 200)
    edges <- forAll $ Gen.int (Range.linear 2 2000)
    graphs <- forAll $ genGraphs (fromIntegral nodes) (fromIntegral edges)

    case G.isConnected (G.merges1 graphs) of
        False -> discard
        True -> do
            case G.nodeList (G.merges1 graphs) of
                []     -> discard
                merged -> do
                    query <- forAll $ genConnectedQuery (G.reverseAdjacencyList (G.merges1 graphs))

                    baseline  <- go Baseline query graphs
                    local     <- go (Local mode) query graphs

                    checkAnswers approx local baseline

    where
        go :: (MonadTest m, MonadIO m) => Implementation -> ST.Query Natural -> [G.Graph Natural Double] -> m [Double]
        go m query gs = singleTarget m gs query.sources query.target


tolerableError :: Double
tolerableError = 0.00000001

approx :: Double -> Double -> Bool
approx x y
    | abs (x - y) < tolerableError = True
    | x == (read "Infinity") && y == (read "Infinity") = True
    | x == (read "-Infinity") && y == (read "-Infinity") = True
    | otherwise = False

-- | Choice between either a `Baseline` inference implementation taken from hackage,
-- or a inference implementation from the `Local` computation library.
data Implementation = Baseline | Local I.Mode

singleTarget :: (MonadTest m, NFData a,  MonadIO m, Show a, Binary a, Typeable a,  H.Hashable a, Ord a) => Implementation -> [G.Graph a Double] -> [a] -> a -> m [Double]
singleTarget Baseline      graphs sources target = pure $ H.singleTarget graphs sources target infinity
singleTarget (Local mode)  graphs sources target
    | Left _ <- result = failure
    | Right r <- result = r
    where
        result = ST.singleTargetConfigSet mode graphs sources target

-- | Tests that graphs that are missing zero cost self loops throw an error.
-- For the reason behind this behaviour, see the documentation of `ST.singleTarget`
prop_p0 :: Property
prop_p0 = unitTest $ do
    forM_ p0Graphs $ \g -> do
        case results g of
            Left ST.MissingZeroCostSelfLoops -> success
            _                                -> failure
    where
        results :: G.Graph Integer Double -> Either ST.Error (Process [Double])
        results graph = ST.singleTarget I.Shenoy [graph] p0Queries.sources p0Queries.target

pX :: Problem -> Property
pX p = unitTest $ do
    forM [Baseline, Local I.BruteForce, Local (I.Fusion False), Local I.Shenoy] $ \mode -> do
        results <- singleTarget mode p.graphs p.q.sources p.q.target
        checkAnswers approx (results) p.answers

prop_p1_drawGraph :: Property
prop_p1_drawGraph = unitTest $ do
    case ST.singleTargetTmp settings p1.graphs p1.q.sources p1.q.target of
        Left _ -> failure
        Right results -> do
            results' <- results
            checkAnswers approx results' p1.answers

    where
        settings = D.def { D.beforeInference = Just "p1_before.svg"
                         , D.afterInference  = Just "p1_after.svg"
                        }

-- prop_p2_drawGraph :: Property
-- prop_p2_drawGraph = unitTest $ do
--     case ST.singleTargetConfigSetDraw "p2.svg" undefined p2.graphs p2.q.sources p2.q.target of
--         Left _ -> failure
--         Right results -> do
--             results' <- results
--             checkAnswers approx results' p2.answers

-- | Tests that the all implementations work for a set problem where one graph is given.
prop_p1 :: Property
prop_p1 = pX p1

-- | Tests that the all implementations work for a set problem where multiple graphs are given.
prop_p2 :: Property
prop_p2 = pX p2

-- | Checks the output of the local computation algorithms and the baseline algorithm match for a set of random queries.
matchesBaseline :: forall a . (NFData a, H.Hashable a, Binary a, Typeable a, Show a, Ord a)
    => I.Mode
    -> G.Graph a Double
    -> TestLimit
    -> Property
matchesBaseline mode g numTests = withTests numTests . property $ do
    query <- forAll $ genConnectedQuery reverseAdjacencyList

    baseline  <- go Baseline query
    local     <- go (Local mode) query

    checkAnswers approx local baseline

    where
        go :: (MonadTest m, MonadIO m) => Implementation -> ST.Query a -> m [Double]
        go m query = singleTarget m [g] query.sources query.target

        reverseAdjacencyList = G.reverseAdjacencyList g

-- | Parses the given graph. Fails if a parse error occurs.
parseGraph :: IO (Either P.ParseError (Either P.InvalidGraphFile a)) -> PropertyT IO a
parseGraph g = do
    parsed <- liftIO g
    case parsed of
        Left e  -> do annotateShow e; failure
        Right parseResult -> case parseResult of
            Left e  -> do annotateShow e; failure
            Right x -> pure x

-- | Tests the parser doesn't fail on a known working example.
prop_parser :: Property
prop_parser = unitTest $ do
    _ <- parseGraph p3SmallGraph
    parseGraph p3MediumGraph
