{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Tests.ShortestPath.SingleTarget
    ( tests )
where

import qualified Benchmark.Baseline.DjikstraSimple                            as H
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
import           Control.Distributed.Process                                  (Process,
                                                                               liftIO)
import           Control.Distributed.Process.Serializable                     (Serializable)
import           Control.Monad                                                (forM,
                                                                               forM_)
import           Data.Functor                                                 (void)
import qualified Data.Map                                                     as M'
import qualified Data.Set                                                     as S
import qualified LocalComputation.Instances.ShortestPath.Parser               as P
import qualified LocalComputation.ValuationAlgebra.QuasiRegular.SemiringValue as Q
import           Numeric.Natural                                              (Natural)
import           System.IO.Silently                                           (capture)
import qualified Text.Parsec                                                  as P

tests :: IO Bool
tests = checkSequential $$(discover)

tolerableError :: TropicalSemiringValue
tolerableError = 0.00000001

approx :: TropicalSemiringValue -> TropicalSemiringValue -> Bool
approx x y = abs (x - y) < tolerableError

prop_p1 :: Property
prop_p1 = withTests 1 . property $ do
    results <- liftIO $ runProcessLocal $ ST.answerQueries [p1Graph] (fst p1Queries) (snd p1Queries)
    checkAnswers approx results p1Answers

-- The hackage version is single source, while our implemented version is single target. Here, we will
-- act like the hackage version is performing single target, but only test on graphs where the path weights
-- are undirected, because otherwise they will not be equivalent.
prop_prebuilt :: Property
prop_prebuilt = withTests 1 . property $ do
    checkAnswers approx answers p1Answers

    where
        answers = H.answerQueries (H.create p1Graph) (snd p1Queries) (fst p1Queries)

genQuery :: (Ord a) => S.Set a -> Gen ([a], a)
genQuery vertices
    | null vertices = error "Expected non-empty vertices iterable"
    | otherwise = do
    target <- Gen.element vertices
    sources <- Gen.set (Range.linear 1 (length vertices - 1)) (Gen.element vertices)
    pure $ (S.toList sources, target)

prop_inferenceMatchesPrebuilt :: Property
prop_inferenceMatchesPrebuilt = withTests 100 . property $ do
    query <- forAll $ genQuery p1GraphVertices
    inferenceResults <- liftIO $ runProcessLocal $ ST.answerQueries [p1Graph] (fst query) (snd query)
    annotate $ show inferenceResults
    let prebuiltResults = H.answerQueries (H.create p1Graph) (snd query) (fst query)

    checkAnswers approx inferenceResults prebuiltResults
    where
        p1GraphVertices = M'.keysSet (p1Graph :: ST.Graph Integer TropicalSemiringValue)

parseGraph :: IO (Either P.ParseError a) -> PropertyT IO a
parseGraph g = do
    parsed <- liftIO g
    case parsed of
        Left e  -> do annotateShow e; failure
        Right x -> pure x

prop_parser :: Property
prop_parser = withTests 1 . property $ do
    g <- parseGraph p2Graph
    case g of
        Left x  -> do annotateShow x; failure
        Right _ -> success
