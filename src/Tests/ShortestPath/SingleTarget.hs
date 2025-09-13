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
    checkParallel $ Group "Tests.ShortestPath.SingleTarget" $ map (getTest p3VerySmall) [Local I.BruteForce, Local I.Fusion, Local I.Shenoy, DP]

    where
        getTest :: G.Graph Natural Double -> Implementation -> (PropertyName, Property)
        getTest g mode = (Hedgehog.PropertyName $ "prop_p3VerySmall_MatchesBaseline_" ++ show mode
                         , matchesBaseline mode g 100)

-- TODO: Clean up
randomMatchesBaseline :: IO Bool
randomMatchesBaseline = do
    checkParallel $ Group "Tests.ShortestPath.SingleTarget" $ map getTest [Local I.BruteForce, Local I.Fusion, Local I.Shenoy, DP]

    where
        getTest :: Implementation -> (PropertyName, Property)
        getTest mode = (Hedgehog.PropertyName $ "prop_random_MatchesBaseline_" ++ show mode
                      , randomMatchesBaseline' mode 100)

-- TODO: Something doesn't like empty graphs...
randomMatchesBaseline' :: Implementation -> TestLimit -> Property
randomMatchesBaseline' mode numTests = withTests numTests . property $ do
    nodes <- forAll $ Gen.int (Range.linear 2 200)
    edges <- forAll $ Gen.int (Range.linear 2 2000)
    graphs <- forAll $ genGraphs (fromIntegral nodes) (fromIntegral edges)

    query <- forAll $ genConnectedQuery (G.reverseAdjacencyList (G.merges1 graphs))

    if not $ G.isConnected (G.merges1 graphs) then discard else pure ()

    if G.nodeList (G.merges1 graphs) == [] then discard else pure ()


    baseline  <- go Baseline query graphs
    local     <- case not (G.isConnected (G.merges1 graphs)) && mode == DP of
                    True -> case resultNow graphs query of
                                Left _  -> failure
                                Right x -> liftIO $ x
                    False -> go mode query graphs

    checkAnswers approx local baseline

    where
        go :: (MonadTest m, MonadIO m) => Implementation -> ST.Query Natural -> [G.Graph Natural Double] -> m [Double]
        go m query gs = singleTarget m gs query.sources query.target

        -- TODO: Remove debugging code.
        resultNow :: (NFData a, Show a, Binary a, Typeable a, H.Hashable a, Ord a) => [G.Graph a Double] -> ST.Query a -> Either I.Error (IO [Double])
        resultNow graphs query = ST.singleTargetDP s graphs query

        s = D.def { D.afterInference = Just "diagrams/unconnected.svg" }


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
data Implementation = Baseline | Local I.Mode | DP deriving (Show, Eq)

singleTarget :: (MonadTest m, NFData a,  MonadIO m, Show a, Binary a, Typeable a,  H.Hashable a, Ord a) => Implementation -> [G.Graph a Double] -> [a] -> a -> m [Double]
singleTarget Baseline      graphs sources target = pure $ H.singleTarget graphs sources target infinity
singleTarget (Local mode)  graphs sources target = case ST.singleTarget D.def mode graphs (ST.Query sources target) of
                                                        Left _  -> failure
                                                        Right x -> x
singleTarget DP            graphs sources target = case ST.singleTargetDP D.def graphs (ST.Query sources target) of
                                                        Left _  -> failure
                                                        Right x -> x

pX :: Problem -> Property
pX p = unitTest $ do
    forM [Baseline, Local I.BruteForce, Local I.Fusion, Local I.Shenoy] $ \mode -> do
        results <- singleTarget mode p.graphs p.q.sources p.q.target
        checkAnswers approx (results) p.answers

prop_p1_drawGraph :: Property
prop_p1_drawGraph = unitTest $ do
    case ST.singleTarget settings I.Shenoy p1.graphs p1.q of
        Left _ -> failure
        Right results -> do
            results' <- results
            checkAnswers approx results' p1.answers

    where
        settings = D.def { D.beforeInference = Just "diagrams/p1_before.svg"
                         , D.afterInference  = Just "diagrams/p1_after.svg"
                        }

prop_p2_drawGraph :: Property
prop_p2_drawGraph = unitTest $ do
    case ST.singleTargetDP settings p2.graphs p2.q of
        Left _ -> failure
        Right results -> do
            results' <- results
            checkAnswers approx results' p2.answers
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

-- | Checks the output of the local computation algorithms and the baseline algorithm match for a set of random queries.
matchesBaseline :: forall a . (NFData a, H.Hashable a, Binary a, Typeable a, Show a, Ord a)
    => Implementation
    -> G.Graph a Double
    -> TestLimit
    -> Property
matchesBaseline mode g numTests = withTests numTests . property $ do
    query <- forAll $ genConnectedQuery reverseAdjacencyList

    baseline  <- go Baseline query
    local     <- go mode query

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
