{-# LANGUAGE TemplateHaskell #-}

module Tests.FastFourierTransform
    ( tests )
where

import           Benchmarks.FastFourierTransform.Baseline
import           Benchmarks.FastFourierTransform.Data
import           LocalComputation.Instances.FastFourierTransform
import           LocalComputation.LocalProcess
import           LocalComputation.Utils

import           Hedgehog
import qualified Hedgehog.Gen                                    as Gen
import qualified Hedgehog.Range                                  as Range

import           Control.Distributed.Process                     (liftIO)

import           Data.Complex                                    (Complex ((:+)),
                                                                  imagPart,
                                                                  realPart)
import qualified LocalComputation.Inference                      as I
import           Tests.Utils                                     (checkAnswers,
                                                                  unitTest)

tests :: IO Bool
tests = checkParallel $$(discover)

tolerableError :: Double
tolerableError = 0.00000001

approximateEquals :: FourierComplex -> FourierComplex -> Bool
approximateEquals (FourierComplex x) (FourierComplex y) = abs (realPart x - realPart y) < tolerableError && abs (imagPart x - imagPart y) < tolerableError

-- | Tests implementation a random test case.
prop_queryMatchesKnownAnswers :: Property
prop_queryMatchesKnownAnswers = unitTest $ do

    case query I.Fusion fourierP1Samples fourierP1Queries of
        Nothing       -> failure
        Just resultsM -> do
            results <- run resultsM
            checkAnswers approximateEquals results fourierP1Answers

-- | Tests implementation on random samples.
prop_matchesHackagePackage :: Property
prop_matchesHackagePackage = withTests 100 . property $ do

    -- Get random list of samples. Making this too large makes inference take too long.
    i <- forAll $ Gen.int (Range.linear 1 4)
    samples <- forAll $ Gen.list (Range.singleton (listOfPowersOfTwo !! i)) $
                        Gen.double (Range.exponentialFloat (-100000) 100000)
    let samples' = map (\x -> FourierComplex $ x :+ 0) samples

    -- Hackage package results
    answers <- liftIO $ dft samples'

    case query I.Fusion samples' [0 .. (fromIntegral $ length samples' - 1)] of
        Nothing       -> failure
        Just resultsM -> do
            results <- run resultsM
            checkAnswers approximateEquals results (map FourierComplex answers)

-- | Tests implementation on [0..]
prop_matchesHackagePackage2 :: Property
prop_matchesHackagePackage2 = unitTest $ do

    let samples' = map (\x -> FourierComplex $ x :+ 0) samples

    answers <- liftIO $ dft samples'
    case query I.Fusion samples' [0 .. (fromIntegral $ length samples' - 1)] of
        Nothing       -> failure
        Just resultsM -> do
            results <- run resultsM
            checkAnswers approximateEquals results (map FourierComplex answers)

    where
        samples = take (2 ^ (5 :: Int)) $ repeat 0

