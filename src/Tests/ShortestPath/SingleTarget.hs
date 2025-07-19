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
      , p3MatchesPrebuilt
   ]

p3MatchesPrebuilt :: IO Bool
p3MatchesPrebuilt = do
    g <- parseGraphWithSelfLoops' p3Graph
    checkSequential $ Group "Tests.ShortestPath.SingleTarget" [
        ("prop_p3matchesPrebuilt", matchesPrebuilt g)
       ]

tolerableError :: TropicalSemiringValue
tolerableError = 0.00000001

approx :: TropicalSemiringValue -> TropicalSemiringValue -> Bool
approx x y
    | abs (x - y) < tolerableError = True
    | x == (T $ read "Infinity") && y == (T $ read "Infinity") = True
    | x == (T $ read "-Infinity") && y == (T $ read "-Infinity") = True
    | otherwise = False

prop_p0 :: Property
prop_p0 = unitTest $ do
    forM_ p0Graphs $ \g -> do
        results <- liftIO $ runProcessLocal $ ST.singleTarget [g] (fst p0Queries) (snd p0Queries)
        case results of
            Left ST.MissingZeroCostSelfLoops -> success
            Right _                          -> failure

prop_p1 :: Property
prop_p1 = withTests 1 . property $ do
    results <- fmap fromRight $ liftIO $ runProcessLocal $ ST.singleTarget [p1Graph] (fst p1Queries) (snd p1Queries)
    checkAnswers approx results p1Answers

prop_prebuilt :: Property
prop_prebuilt = withTests 1 . property $ do
    checkAnswers approx answers p1Answers

    where
        answers = H.singleTarget p1Graph (fst p1Queries) (snd p1Queries) (T $ read "Infinity")

genQuery :: (Ord a) => S.Set a -> Gen ([a], a)
genQuery vertices
    | null vertices = error "Expected non-empty vertices iterable"
    | otherwise = do
    target <- Gen.element vertices
    sources <- Gen.set (Range.linear 1 (length vertices - 1)) (Gen.element vertices)
    pure $ (S.toList sources, target)

matchesPrebuilt :: (Binary a, Typeable a, Show a, Ord a)
    => G.Graph a TropicalSemiringValue
    -> Property
matchesPrebuilt g = withTests 100 . property $ do
    query <- forAll $ genQuery (G.nodes g)

    inferenceResults <- fmap fromRight $ liftIO $ runProcessLocal $ ST.singleTarget [g] (fst query) (snd query)
    let prebuiltResults =                                            H.singleTarget g (fst query) (snd query) (T $ read "Infinity")

    checkAnswers approx inferenceResults prebuiltResults


{- | Parses the graph and adds self loops of cost 0.

The addition of these self loops is necessary as not all implementations of shortest path
algorithms assume the existence of 0 cost self loops as the quasi-regular inference
based implementation does.
-}
parseGraphWithSelfLoops :: (Ord a, Num b)
    => IO (Either P.ParseError (Either P.InvalidGraphFile (G.Graph a b)))
    -> PropertyT IO (G.Graph a b)
parseGraphWithSelfLoops = fmap (G.addSelfLoops 0) . parseGraph

-- | Unsafe version of `parseGraphWithSelfLoops`.
parseGraphWithSelfLoops' :: (Ord a, Num b)
    => IO (Either P.ParseError (Either P.InvalidGraphFile (G.Graph a b)))
    -> IO (G.Graph a b)
parseGraphWithSelfLoops' = fmap (G.addSelfLoops 0) . parseGraph'

-- | Parses the given graph. Fails if a parse error occurs.
parseGraph :: IO (Either P.ParseError (Either P.InvalidGraphFile a)) -> PropertyT IO a
parseGraph g = do
    parsed <- liftIO g
    case parsed of
        Left e  -> do annotateShow e; failure
        Right parseResult -> case parseResult of
            Left e  -> do annotateShow e; failure
            Right x -> pure x

-- | Unsafe version of `parseGraph`
parseGraph' :: IO (Either P.ParseError (Either P.InvalidGraphFile a)) -> IO a
parseGraph' = fmap (fromRight . fromRight)

prop_parser :: Property
prop_parser = unitTest $ parseGraph p3Graph
