{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Tests the valuation algebra solving shortest path problems.
-- Also doubles up to test that all methods of inference produce
-- the same results - all tests verify this property.
module Tests.ShortestPath.SingleTarget
    ( tests )
where

import qualified Benchmark.Baseline.DjikstraSimple                            as H
import qualified LocalComputation.Graph                                       as G
import qualified LocalComputation.Instances.ShortestPath.SingleTarget         as ST
import           LocalComputation.Utils
import           LocalComputation.ValuationAlgebra.QuasiRegular
import           Tests.ShortestPath.SingleTarget.Data

import           Hedgehog
import qualified Hedgehog.Gen                                                 as Gen
import qualified Hedgehog.Range                                               as Range

import           Control.Distributed.Process                                  (Process,
                                                                               liftIO)
import           Control.Monad                                                (forM_)
import qualified Data.Set                                                     as S
import qualified LocalComputation.Instances.ShortestPath.Parser               as P
import qualified Text.Parsec                                                  as P

-- Typeclasses
import           Control.DeepSeq                                              (NFData)
import           Control.Monad.IO.Class                                       (MonadIO)
import           Data.Binary                                                  (Binary)
import qualified Data.Hashable                                                as H
import qualified LocalComputation.Inference                                   as I
import           LocalComputation.ValuationAlgebra.QuasiRegular.SemiringValue (toDouble)
import           Type.Reflection                                              (Typeable)

tests :: IO Bool
tests = fmap and $ sequence [
        checkParallel $$(discover)
      , p3MatchesPrebuilt
   ]

p3MatchesPrebuilt :: IO Bool
p3MatchesPrebuilt = do
    p3VerySmall <- P.fromValid p3VerySmallGraph
    --p3Small <- P.fromValid p3SmallGraph
    checkSequential $ Group "Tests.ShortestPath.SingleTarget" [
            ("prop_p3VerySmallMatchesPrebuilt", matchesPrebuilt p3VerySmall 30)
          , ("prop_p3VerySmallMatchesPrebuiltFusion", matchesPrebuiltFusion p3VerySmall 500)
        --, ("prop_p3SmallMatchesPrebuilt", matchesPrebuilt p3Small 1)
       ]

tolerableError :: Double
tolerableError = 0.00000001

approx :: Double -> Double -> Bool
approx x y
    | abs (x - y) < tolerableError = True
    | x == (read "Infinity") && y == (read "Infinity") = True
    | x == (read "-Infinity") && y == (read "-Infinity") = True
    | otherwise = False

singleTarget :: (MonadTest m, NFData a,  MonadIO m, Show a, Binary a, Typeable a,  H.Hashable a, Ord a) => I.Mode -> [G.Graph a TropicalSemiringValue] -> [a] -> a -> m [TropicalSemiringValue]
singleTarget mode graph sources target
    | Left _ <- result = failure
    | Right r <- result = r
    where
        result = ST.singleTarget mode graph sources target

-- | Tests that graphs that are missing zero cost self loops throw an error.
-- For the reason behind this behaviour, see the documentation of `ST.singleTarget`
prop_p0 :: Property
prop_p0 = unitTest $ do
    forM_ p0Graphs $ \g -> do
        case results g of
            Left ST.MissingZeroCostSelfLoops -> success
            _                                -> failure
    where
        results :: G.Graph Integer TropicalSemiringValue -> Either ST.Error (Process [TropicalSemiringValue])
        results graph = ST.singleTarget I.Shenoy [graph] p0Queries.sources p0Queries.target

-- | Tests that the localcomputation algorithm works for a set problem, where one graph is given.
prop_p1 :: Property
prop_p1 = unitTest $ do
    results <- singleTarget I.Shenoy p1.graphs p1.q.sources p1.q.target
    checkAnswers approx (map toDouble results) p1.answers

prop_p1fusion :: Property
prop_p1fusion = unitTest $ do
    result <- singleTarget I.Fusion p1.graphs p1.q.sources p1.q.target
    checkAnswers approx (map toDouble result) p1.answers

-- | Tests that the localcomputation algorithm works for a set problem, where multiple graphs are given.
prop_p2 :: Property
prop_p2 = unitTest $ do
    results <- singleTarget I.Shenoy p2.graphs p2.q.sources p2.q.target
    checkAnswers approx (map toDouble results) p2.answers

prop_p2fusion :: Property
prop_p2fusion = unitTest $ do
    result <- singleTarget I.Fusion p2.graphs p2.q.sources p2.q.target
    checkAnswers approx (map toDouble result) p2.answers

-- | Tests that the baseline algorithm works for a set problem.
prop_prebuilt :: Property
prop_prebuilt = withTests 1 . property $ do
    checkAnswers approx results p1.answers

    where
        results = H.singleTarget p1.graphs p1.q.sources p1.q.target infinity

-- | Generates a random query from the given set of graph vertices.
genQuery :: (Ord a) => S.Set a -> Gen (Query a)
genQuery vertices
    | null vertices = error "Expected non-empty vertices iterable"
    | otherwise = do
    target <- Gen.element vertices
    sources <- Gen.set (Range.linear 1 (length vertices - 1)) (Gen.element vertices)
    pure $ Query (S.toList sources) target

-- | Checks the output of the localcomputation algorithm and the baseline algorithm match for a set of random queries.
matchesPrebuilt :: (NFData a, H.Hashable a, Binary a, Typeable a, Show a, Ord a)
    => G.Graph a Double
    -> TestLimit
    -> Property
matchesPrebuilt g numTests = withTests numTests . property $ do
    query <- forAll $ genQuery (G.nodes g)

    let prebuiltResults =                       H.singleTarget [g] query.sources query.target infinity
    if all (== infinity) prebuiltResults then discard else pure ()
    inferenceResults <- singleTarget I.Shenoy [fmap T g] query.sources query.target

    checkAnswers approx (map toDouble inferenceResults) prebuiltResults

matchesPrebuiltFusion :: (NFData a, H.Hashable a, Binary a, Typeable a, Show a, Ord a)
    => G.Graph a Double
    -> TestLimit
    -> Property
matchesPrebuiltFusion g numTests = withTests numTests . withDiscards 10000 . property $ do
    query <- forAll $ genQuery (G.nodes g)

    let prebuiltResults =             H.singleTarget [g] query.sources query.target infinity
    if all (== infinity) prebuiltResults then discard else pure ()
    result <- singleTarget I.Fusion [(fmap T g)] query.sources query.target

    checkAnswers approx (map toDouble result) prebuiltResults

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
prop_parser = unitTest $ parseGraph p3SmallGraph
