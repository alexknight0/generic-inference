{-# LANGUAGE TemplateHaskell #-}

module Tests.FastFourierTransform
    ( tests )
where

import           Benchmark.Baseline.FFT
import           LocalComputation.Instances.FastFourierTransform
import           LocalComputation.LocalProcess
import           LocalComputation.Utils
import           Tests.FastFourierTransform.Data

import           Hedgehog
import qualified Hedgehog.Gen                                    as Gen
import qualified Hedgehog.Range                                  as Range

import           Control.Distributed.Process                     (liftIO)

import           Data.Complex                                    (Complex ((:+)),
                                                                  imagPart,
                                                                  realPart)
import           Tests.Utils                                     (checkAnswers,
                                                                  unitTest)

tests :: IO Bool
tests = checkParallel $$(discover)

tolerableError :: Double
tolerableError = 0.00000001

approximateEquals :: FourierComplex -> FourierComplex -> Bool
approximateEquals (FourierComplex x) (FourierComplex y) = abs (realPart x - realPart y) < tolerableError && abs (imagPart x - imagPart y) < tolerableError

prop_queryMatchesKnownAnswers :: Property
prop_queryMatchesKnownAnswers = unitTest $ do

    resultsP1 <- run $ query fourierP1Samples fourierP1Queries
    case resultsP1 of
        Nothing -> failure
        Just xs -> checkAnswers approximateEquals xs fourierP1Answers

    resultsP2 <- run $ query fourierP2Samples fourierP2Queries
    case resultsP2 of
        Nothing -> failure
        Just xs -> checkAnswers approximateEquals xs fourierP2Answers

prop_matchesHackagePackage :: Property
prop_matchesHackagePackage = withTests 100 . property $ do

    -- Get random list of samples. Making this too large makes inference take too long.
    i <- forAll $ Gen.int (Range.linear 1 4)
    samples <- forAll $ Gen.list (Range.singleton (listOfPowersOfTwo !! i)) $
                        Gen.double (Range.exponentialFloat (-100000) 100000)
    let samples' = map (\x -> FourierComplex $ x :+ 0) samples

    answers <- liftIO $ dft samples'
    results <- run $ query samples' [0 .. (fromIntegral $ length samples' - 1)]

    case results of
        Nothing -> failure
        Just xs -> checkAnswers approximateEquals xs (map FourierComplex answers)

