{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module UtilsTests
    ( tests )
where

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

tests :: IO Bool
tests = checkParallel $$(discover)

prop_binaryConversion :: Property
prop_binaryConversion = withTests 100 . property $ do
    x <- forAll $ Gen.int (Range.linear 0 100000)
    numLeadingZeroes <- forAll $ Gen.int (Range.linear 0 10)
    diff x (==) (convertBackToInt $ reverse $ toBinaryLeadingZeroes (fromIntegral numLeadingZeroes) (fromIntegral x))

    where
        -- Assumes first bit is LSB
        convertBackToInt :: [Bool] -> Int
        convertBackToInt xs = sum $ zipWith (\x y -> fromEnum x * y) xs listOfMultiplesOfTwo

        listOfMultiplesOfTwo :: [Int]
        listOfMultiplesOfTwo = 1 : (map (*2) listOfMultiplesOfTwo)


