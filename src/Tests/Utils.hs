{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

{- | Module containing both tests for the utilities in the local computation library,
as well as general utilities used by the test library.
-}
module Tests.Utils (
      tests
    , genMode
    , unitTest
    , checkAnswers
) where

import           Hedgehog
import qualified Hedgehog                                  as H (assert)
import qualified Hedgehog.Gen                              as Gen
import qualified Hedgehog.Range                            as Range

import qualified Algebra.Graph.Undirected                  as UG
import qualified Benchmarks.ShortestPath.SingleTarget.Data as ST
import           Control.DeepSeq                           (force)
import           Control.Distributed.Process
import qualified Control.Exception                         as E
import           Control.Monad                             (void)
import qualified Data.Set                                  as S
import qualified LocalComputation.Graph                    as UG (toAlgebraGraph')
import qualified LocalComputation.Graph.Undirected         as G
import qualified LocalComputation.Inference                as I
import qualified LocalComputation.Inference.MessagePassing as MP
import qualified LocalComputation.Inference.Triangulation  as T
import           LocalComputation.Utils                    (zipWithAssert)
import qualified LocalComputation.Utils                    as U

-------------------------------------------------------------------------------
-- Tests                                                                     --
-------------------------------------------------------------------------------
tests :: IO Bool
tests = checkParallel $$(discover)

prop_assertsAreStillPresent :: Property
prop_assertsAreStillPresent = withTests 100 . property $ do
    footnote "If this test fails, then optimizations are removing some useful \
              \assertions that should probably be active during testing. \
              \Try using `stack test --fast` (disables all optimizations)."
    x <- forAll $ Gen.int (Range.linear 1 100)
    result <- liftIO $ E.try $ E.evaluate $ force (f x == 4)
    case result of
        Left (E.AssertionFailed _) -> success
        Right _                    -> failure

    where
        -- If optimizations are enabled such that this assert is not triggered,
        -- then other asserts in the main code will also have been removed.
        f :: Int -> Int
        f _ | E.assert False False = undefined
        f _ = 4

prop_combinations :: Property
prop_combinations = withTests 100 . property $ do
    n <- forAll $ Gen.int (Range.linear 0 12)
    r <- forAll $ Gen.int (Range.linear 0 n)
    xs <- forAll $ Gen.list (Range.singleton n)
                            (Gen.int $ Range.linear 1 10)

    let nChooseR = div (factorial n)
                       (factorial r * factorial (n - r))

    length (U.combinations r xs) === nChooseR

    where
        factorial n = product [1..n]

prop_isComplete :: Property
prop_isComplete = unitTest $ do

    G.isComplete k0 === True
    G.isComplete k1 === True
    G.isComplete k2 === True
    G.isComplete k3 === True
    G.isComplete notK2 === False
    G.isComplete notK3 === False

    where
        k0 :: G.Graph Int
        k0 = G.fromList []

        k1 :: G.Graph Int
        k1 = G.fromList [(1, [])]

        k2 :: G.Graph Int
        k2 = G.fromList [ (1, [2])
                              , (2, [1])
                             ]

        k3 :: G.Graph Int
        k3 = G.fromList [ (1, [2, 3])
                              , (2, [1, 3])
                              , (3, [1, 2])
                             ]

        notK2 :: G.Graph Int
        notK2 = G.fromList [ (1, [])
                                 , (2, [])
                                ]

        notK3 :: G.Graph Int
        notK3 = G.fromList [ (1, [2])
                                 , (2, [1])
                                 , (3, [])
                                ]

prop_triangulationLoneVertex :: Property
prop_triangulationLoneVertex = unitTest $ do
    graph <- fmap G.fromGraph $ forAll $ ST.genGraph 1 0
    T.triangulate graph === graph

prop_triangulation :: Property
prop_triangulation = withTests 100 . property $ do
    nodes <- forAll $ Gen.int (Range.linear 1 50)   -- There is no valid query for 0 nodes.
    edges <- forAll $ Gen.int (Range.linear 0 500)
    graph <- fmap G.fromGraph $ forAll $ ST.genGraph (fromIntegral nodes) (fromIntegral edges)

    let triangulated = T.triangulate graph

    G.vertexSet triangulated === G.vertexSet graph
    H.assert $ S.isSubsetOf (G.edgeSet graph) (G.edgeSet triangulated)



-------------------------------------------------------------------------------
-- Utils                                                                     --
-------------------------------------------------------------------------------

genMode :: Gen I.Mode
genMode = Gen.element [I.BruteForce, I.Fusion, (I.Shenoy MP.Threads)]

unitTest :: PropertyT IO a -> Property
unitTest = withTests 1 . property . void

checkAnswers :: (Show a, Show b) => (a -> b -> Bool) -> [a] -> [b] -> PropertyT IO ()
checkAnswers f results answers = diff results (\rs as -> and (zipWithAssert f rs as)) answers

