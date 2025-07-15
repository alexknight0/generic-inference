{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Tests.FourierTests
    ( tests )
where

import           Tests.TestData
import           Instances.FastFourierTransform
import           LocalProcess
import           ValuationAlgebra.Semiring

import           Hedgehog
import qualified Hedgehog.Gen                             as Gen
import qualified Hedgehog.Range                           as Range

import           Control.Concurrent                       (threadDelay)
import           Control.Distributed.Process
import           Control.Distributed.Process.Node
import           Control.Distributed.Process.Serializable (Serializable)
import           Data.Functor                             (void)
import           System.IO.Silently                       (capture)
import           Utils

import           Data.Array.CArray                        (createCArray)
import           Data.Array.CArray.Base                   (CArray)
import qualified Data.Array.IArray                        as I
import           Data.Complex                             (Complex ((:+)),
                                                           imagPart, realPart)
import           Math.FFT                                 (dft)

tests :: IO Bool
tests = checkSequential $$(discover)

tolerableError :: Double
tolerableError = 0.00000001

approximateEquals :: FourierComplex -> FourierComplex -> Bool
approximateEquals (FourierComplex x) (FourierComplex y) = abs (realPart x - realPart y) < tolerableError && abs (imagPart x - imagPart y) < tolerableError

prop_queryMatchesKnownAnswers :: Property
prop_queryMatchesKnownAnswers = unitTest $ do

    resultsP1 <- liftIO $ runProcessLocal $ query fourierP1Samples fourierP1Queries
    case resultsP1 of
        Nothing -> failure
        Just xs -> checkAnswers approximateEquals xs fourierP1Answers

    resultsP2 <- liftIO $ runProcessLocal $ query fourierP2Samples fourierP2Queries
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

    answers <- liftIO $ fmap dft (createComplexArray samples')
    results <- liftIO $ runProcessLocal $ query samples' [0 .. (fromIntegral $ length samples' - 1)]

    case results of
        Nothing -> failure
        Just xs -> checkAnswers approximateEquals xs (map (FourierComplex) $ I.elems answers)
