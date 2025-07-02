{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module FourierTests
    ( tests )
where

import           Data
import           LocalProcess
import           SemiringValuationAlgebra

import           Hedgehog
import qualified Hedgehog.Gen                             as Gen
import qualified Hedgehog.Range                           as Range

import           Control.Concurrent                       (threadDelay)
import           Control.Distributed.Process
import           Control.Distributed.Process.Node
import           Control.Distributed.Process.Serializable (Serializable)
import           Data.Functor                             (void)
import           FastFourierTransform
import           System.IO.Silently                       (capture)
import           Utils

import           Data.Complex         (Complex ((:+)), realPart, imagPart)
import Data.Array.CArray (createCArray)
import Data.Array.CArray.Base (CArray)
import           Math.FFT             (dft)
import qualified Data.Array.IArray as I

tests :: IO Bool
tests = checkSequential $$(discover)

tolerableError = 0.00000001

approximateEquals :: FourierComplex -> FourierComplex -> Bool
approximateEquals (FourierComplex x) (FourierComplex y) = abs (realPart x - realPart y) < tolerableError && abs (imagPart x - imagPart y) < tolerableError

prop_queryMatchesKnownAnswers :: Property
prop_queryMatchesKnownAnswers = unitTest $ do
    resultsP1 <- liftIO $ runProcessLocal $ query fourierP1Samples fourierP1Queries
    checkAnswers approximateEquals fourierP1Answers resultsP1
    resultsP2 <- liftIO $ runProcessLocal $ query fourierP2Samples fourierP2Queries
    checkAnswers approximateEquals fourierP2Answers resultsP2

prop_matchesHackagePackage :: Property
prop_matchesHackagePackage = withTests 30 . property $ do
    samples <- forAll $ Gen.filterT (\xs -> length xs `elem` [2,4,8,16,32,64,128,256,512]) $ Gen.list (Range.linear 0 100) $ Gen.double (Range.exponentialFloat (-100000) 100000)
    let samples' = map (\x -> FourierComplex $ x :+ 0) samples
    results <- liftIO $ runProcessLocal $ query samples' [0 .. (fromIntegral $ length samples' - 1)]
    answers <- liftIO $ fmap dft (createComplexArray samples')
    checkAnswers approximateEquals (map (FourierComplex) $ I.elems answers) results
