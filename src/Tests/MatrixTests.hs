{-# LANGUAGE TemplateHaskell     #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tests.MatrixTests
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
import           Data.Maybe                               (isNothing)
import           Debug.Trace                              (traceShow,
                                                           traceShowId)
import           GHC.Float                                (properFractionDouble)
import           System.IO.Silently                       (capture)
import           Utils

import qualified Data.Matrix                              as M
import qualified LabelledMatrix                           as L

tests :: IO Bool
tests = checkParallel $$(discover)

genMatrix :: forall a. Int -> Int -> Gen a -> Gen (M.Matrix a)
genMatrix numRows numColumns genA = fmap M.fromLists $ genListOfRows
    where
        genListOfRows :: Gen [[a]]
        genListOfRows = Gen.list (Range.singleton numRows) genRow

        -- A row has numColumns entries.
        genRow :: Gen [a]
        genRow = Gen.list (Range.singleton numColumns) genA

prop_matrixAddition :: Property
prop_matrixAddition = withTests 100 . property $ do
    [aSize, bSize] <- forAll $ Gen.list (Range.singleton 2) genMatrixSideLength

    m1 <- forAll $ genMatrix aSize bSize genMatrixEntry
    m2 <- forAll $ genMatrix aSize bSize genMatrixEntry

    let answer = m1 `dataMatrixAdd` m2
    let result = L.fromMatrix m1 `add` L.fromMatrix m2

    case result of
        Nothing      -> failure
        Just result' -> diff (L.fromMatrix answer) (==) result'

    where
        genMatrixEntry :: Gen Int
        genMatrixEntry = Gen.int (Range.linear 0 100000)

        -- Seems can't create empty matrix with Data.Matrix
        genMatrixSideLength :: Gen Int
        genMatrixSideLength = Gen.int (Range.linear 1 6)

        dataMatrixAdd = M.elementwise (+)
        add = L.add (+)

prop_matrixMultiplication :: Property
prop_matrixMultiplication = withTests 100 . property $ do
    [aSize, bSize, cSize] <- forAll $ Gen.list (Range.singleton 3) genMatrixSideLength

    m1 <- forAll $ genMatrix aSize bSize genMatrixEntry
    m2 <- forAll $ genMatrix bSize cSize genMatrixEntry

    let answer = m1 `M.multStd` m2
    let result = L.fromMatrix m1 `multiply` L.fromMatrix m2

    case result of
        Nothing      -> failure
        Just result' -> diff (L.fromMatrix answer) (==) result'

    where
        genMatrixEntry :: Gen Int
        genMatrixEntry = Gen.int (Range.linear 0 100000)

        -- Seems can't create empty matrix with Data.Matrix
        genMatrixSideLength :: Gen Int
        genMatrixSideLength = Gen.int (Range.linear 1 6)

        multiply = L.multiply (+) (*) 0
