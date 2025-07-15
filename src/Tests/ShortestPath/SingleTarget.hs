{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Tests.ShortestPath.SingleTarget
    ( tests )
where

import qualified LocalComputation.Instances.ShortestPath.HackageVersion as H
import qualified LocalComputation.Instances.ShortestPath.SingleTarget   as ST
import qualified LocalComputation.LabelledMatrix                        as M
import           LocalComputation.LocalProcess
import           LocalComputation.Utils
import           LocalComputation.ValuationAlgebra.QuasiRegular
import           Tests.ShortestPath.SingleTarget.Data

import           Hedgehog
import qualified Hedgehog.Gen                                           as Gen
import qualified Hedgehog.Range                                         as Range

import           Control.Concurrent                                     (threadDelay)
import           Control.Distributed.Process                            (Process,
                                                                         liftIO)
import           Control.Distributed.Process.Serializable               (Serializable)
import           Control.Monad                                          (forM,
                                                                         forM_)
import           Data.Functor                                           (void)
import qualified Data.Map                                               as M'
import qualified Data.Set                                               as S
import           System.IO.Silently                                     (capture)

tests :: IO Bool
tests = checkSequential $$(discover)

tolerableError :: TropicalSemiringValue
tolerableError = 0.00000001

approx :: TropicalSemiringValue -> TropicalSemiringValue -> Bool
approx x y = abs (x - y) < tolerableError

prop_p1 :: Property
prop_p1 = withTests 1 . property $ do
    results <- liftIO $ runProcessLocal $ ST.answerQueries (map M.fromList graphP1) (fst graphQueriesP1) (snd graphQueriesP1)
    checkAnswers approx results graphAnswersP1

-- The hackage version is single source, while our implemented version is single target. Here, we will
-- act like the hackage version is performing single target, but only test on graphs where the path weights
-- are undirected, because otherwise they will not be equivalent.
prop_prebuilt :: Property
prop_prebuilt = withTests 1 . property $ do
    checkAnswers approx answers graphAnswersP1

    where
        answers = H.answerQueries (H.create graphP1) (snd graphQueriesP1) (fst graphQueriesP1)

prop_inferenceMatchesPrebuilt :: Property
prop_inferenceMatchesPrebuilt = withTests 100 . property $ do
    query <- forAll $ genQuery graphP1Vertices
    inferenceResults <- liftIO $ runProcessLocal $ ST.answerQueries (map M.fromList graphP1) (fst query) (snd query)
    annotate $ show inferenceResults
    let prebuiltResults = H.answerQueries (H.create graphP1) (snd query) (fst query)

    checkAnswers approx inferenceResults prebuiltResults
    where
        graphP1Vertices = S.fromList $ map f (concat graphP1)
            where
                f :: ((Integer, Integer), Integer) -> Integer
                f ((x, _), _) = x

genQuery :: (Ord a) => S.Set a -> Gen ([a], a)
genQuery vertices
    | null vertices = error "Expected non-empty vertices iterable"
    | otherwise = do
    target <- Gen.element vertices
    sources <- Gen.set (Range.linear 1 (length vertices - 1)) (Gen.element vertices)
    pure $ (S.toList sources, target)

