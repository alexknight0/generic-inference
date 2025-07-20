{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Tests.ShortestPath.SingleTarget
    ( tests )
where

import qualified Benchmark.Baseline.DjikstraSimple                    as H
import qualified LocalComputation.Graph                               as G
import qualified LocalComputation.Instances.ShortestPath.SingleTarget as ST
import           LocalComputation.LocalProcess
import           LocalComputation.Utils
import           LocalComputation.ValuationAlgebra.QuasiRegular
import           Tests.ShortestPath.SingleTarget.Data

import           Hedgehog
import qualified Hedgehog.Gen                                         as Gen
import qualified Hedgehog.Range                                       as Range

import           Control.Distributed.Process                          (liftIO)
import           Control.Monad                                        (forM_)
import qualified Data.Set                                             as S
import qualified LocalComputation.Instances.ShortestPath.Parser       as P
import qualified Text.Parsec                                          as P

-- Typeclasses
import           Data.Binary                                          (Binary)
import           Type.Reflection                                      (Typeable)

tests :: IO Bool
tests = fmap and $ sequence [
        checkSequential $$(discover)
      , p3MatchesPrebuilt
   ]

p3MatchesPrebuilt :: IO Bool
p3MatchesPrebuilt = do
    p3VerySmall <- P.fromValid p3VerySmallGraph
    p3Small <- P.fromValid p3SmallGraph
    checkSequential $ Group "Tests.ShortestPath.SingleTarget" [
          ("prop_p3VerySmallMatchesPrebuilt", matchesPrebuilt p3VerySmall 4)
        , ("prop_p3SmallMatchesPrebuilt", matchesPrebuilt p3Small 1)
       ]

tolerableError :: TropicalSemiringValue
tolerableError = 0.00000001

approx :: TropicalSemiringValue -> TropicalSemiringValue -> Bool
approx x y
    | abs (x - y) < tolerableError = True
    | x == (T $ read "Infinity") && y == (T $ read "Infinity") = True
    | x == (T $ read "-Infinity") && y == (T $ read "-Infinity") = True
    | otherwise = False

-- | Tests that graphs that are missing zero cost self loops throw an error.
-- For the reason behind this behaviour, see the documentation of `ST.singleTarget`
prop_p0 :: Property
prop_p0 = unitTest $ do
    forM_ p0Graphs $ \g -> do
        results <- liftIO $ runProcessLocal $ ST.singleTarget [g] p0Queries.sources p0Queries.target
        case results of
            Left ST.MissingZeroCostSelfLoops -> success
            Right _                          -> failure

-- | Tests that the localcomputation algorithm works for a set problem, where one graph is given.
prop_p1 :: Property
prop_p1 = unitTest $ do
    results <- fmap fromRight $ liftIO $ runProcessLocal $ ST.singleTarget [p1Graph] p1Queries.sources p1Queries.target
    checkAnswers approx results p1Answers

-- | Tests that the localcomputation algorithm works for a set problem, where multiple graphs are given.
prop_p2 :: Property
prop_p2 = unitTest $ do
    results <- fmap fromRight $ liftIO $ runProcessLocal $ ST.singleTarget p2Graph p2Queries.sources p2Queries.target
    checkAnswers approx results p1Answers

-- | Tests that the baseline algorithm works for a set problem.
prop_prebuilt :: Property
prop_prebuilt = withTests 1 . property $ do
    checkAnswers approx answers p1Answers

    where
        answers = H.singleTarget p1Graph p1Queries.sources p1Queries.target (T $ read "Infinity")

-- | Generates a random query from the given set of graph vertices.
genQuery :: (Ord a) => S.Set a -> Gen (Query a)
genQuery vertices
    | null vertices = error "Expected non-empty vertices iterable"
    | otherwise = do
    target <- Gen.element vertices
    sources <- Gen.set (Range.linear 1 (length vertices - 1)) (Gen.element vertices)
    pure $ Query (S.toList sources) target

-- | Checks the output of the localcomputation algorithm and the baseline algorithm match for a set of random queries.
matchesPrebuilt :: (Binary a, Typeable a, Show a, Ord a)
    => G.Graph a TropicalSemiringValue
    -> TestLimit
    -> Property
matchesPrebuilt g numTests = withTests numTests . property $ do
    query <- forAll $ genQuery (G.nodes g)

    inferenceResults <- fmap fromRight $ liftIO $ runProcessLocal $ ST.singleTarget [g] query.sources query.target
    let prebuiltResults =                                            H.singleTarget g query.sources query.target (T $ read "Infinity")

    checkAnswers approx inferenceResults prebuiltResults

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
