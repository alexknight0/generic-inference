{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Tests.ShortestPath.SingleTarget
    ( tests )
where

import qualified Benchmark.Baseline.DjikstraSimple                            as H
import qualified LocalComputation.Graph                                       as G
import qualified LocalComputation.Instances.ShortestPath.SingleTarget         as ST
import qualified LocalComputation.LabelledMatrix                              as M
import           LocalComputation.LocalProcess
import           LocalComputation.Utils
import           LocalComputation.ValuationAlgebra.QuasiRegular
import           Tests.ShortestPath.SingleTarget.Data

import           Hedgehog
import qualified Hedgehog.Gen                                                 as Gen
import qualified Hedgehog.Range                                               as Range

import           Control.Concurrent                                           (threadDelay)
import qualified Control.Concurrent.CachedIO                                  as C
import           Control.Distributed.Process                                  (Process,
                                                                               liftIO)
import           Control.Distributed.Process.Serializable                     (Serializable)
import qualified Control.Exception                                            as E (assert)
import           Control.Monad                                                (forM,
                                                                               forM_,
                                                                               mzero)
import           Data.Functor                                                 (void)
import qualified Data.Map                                                     as M'
import qualified Data.Set                                                     as S
import qualified LocalComputation.Instances.ShortestPath.Parser               as P
import qualified LocalComputation.ValuationAlgebra.QuasiRegular.SemiringValue as Q
import           Numeric.Natural                                              (Natural)
import           System.IO.Silently                                           (capture)
import qualified Text.Parsec                                                  as P

-- Typeclasses
import           Data.Binary                                                  (Binary)
import           Type.Reflection                                              (Typeable)

tests :: IO Bool
tests = fmap and $ sequence [
      checkSequential $$(discover)
    -- , do
    --     g <- parseGraph p3Graph
    --     checkSequential $ Group "Tests.ShortestPath.SingleTarget" [
    --         ("prop_matchesPrebuilt", matchesPrebuilt g)
    --        ]
   ]

tolerableError :: TropicalSemiringValue
tolerableError = 0.00000001

approx :: TropicalSemiringValue -> TropicalSemiringValue -> Bool
approx x y = abs (x - y) < tolerableError

prop_p1 :: Property
prop_p1 = withTests 1 . property $ do
    results <- liftIO $ runProcessLocal $ ST.answerQueries [p1Graph] (fst p1Queries) (snd p1Queries)
    checkAnswers approx results p1Answers

prop_prebuilt :: Property
prop_prebuilt = withTests 1 . property $ do
    checkAnswers approx answers p1Answers

    where
        answers = H.singleTarget p1Graph (fst p1Queries) (snd p1Queries)

genQuery :: (Ord a) => S.Set a -> Gen ([a], a)
genQuery vertices
    | null vertices = error "Expected non-empty vertices iterable"
    | otherwise = do
    target <- Gen.element vertices
    sources <- Gen.set (Range.linear 1 (length vertices - 1)) (Gen.element vertices)
    pure $ (S.toList sources, target)

matchesPrebuilt :: (Binary a, Typeable a, Show a, Ord a)
    => IO (Either P.ParseError (Either P.InvalidGraphFile (G.Graph a TropicalSemiringValue)))
    -> Property
matchesPrebuilt readGraph = withTests 1 . property $ do
    g <- parseGraphAndCache readGraph
    query <- forAll $ genQuery (G.nodes g)

    inferenceResults    <- liftIO $ runProcessLocal $ ST.answerQueries [g] (fst query) (snd query)
    let prebuiltResults =                              H.singleTarget g (fst query) (snd query)

    checkAnswers approx inferenceResults prebuiltResults

-- prop_p3MatchesPrebuilt :: Property
-- prop_p3MatchesPrebuilt = matchesPrebuilt p3Graph

parseGraphAndCache :: IO (Either P.ParseError (Either P.InvalidGraphFile a)) -> PropertyT IO a
parseGraphAndCache g = parseGraph $ (C.cachedIOWith (\_ _ -> True) g) >>= C.runCached

parseGraph :: IO (Either P.ParseError (Either P.InvalidGraphFile a)) -> PropertyT IO a
parseGraph g = do
    parsed <- liftIO g
    case parsed of
        Left e  -> do annotateShow e; failure
        Right parseResult -> case parseResult of
            Left e  -> do annotateShow e; failure
            Right x -> pure x

prop_parser :: Property
prop_parser = withTests 1 . property . void $ parseGraph p3Graph
