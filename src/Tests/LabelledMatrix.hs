{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Tests.LabelledMatrix
    ( tests )
where

import           Hedgehog
import qualified Hedgehog.Gen                                                 as Gen
import qualified Hedgehog.Range                                               as Range


import qualified Data.Matrix                                                  as M
import           Data.Text.Lazy                                               (unpack)
import           Debug.Pretty.Simple                                          (pTraceShow)
import qualified LocalComputation.LabelledMatrix                              as L
import           LocalComputation.Utils                                       (fromRight,
                                                                               unitTest)
import           LocalComputation.ValuationAlgebra.QuasiRegular               (SemiringValue (..))
import           LocalComputation.ValuationAlgebra.QuasiRegular.SemiringValue (TropicalSemiringValue (..))
import           Text.Pretty.Simple                                           (pShow)

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

-- prop_matrixAddition :: Property
-- prop_matrixAddition = withTests 100 . property $ do
--     [aSize, bSize] <- forAll $ Gen.list (Range.singleton 2) genMatrixSideLength
--
--     m1 <- forAll $ genMatrix aSize bSize genMatrixEntry
--     m2 <- forAll $ genMatrix aSize bSize genMatrixEntry
--
--     let answer = m1 `dataMatrixAdd` m2
--     let result = L.fromMatrix m1 `L.add` L.fromMatrix m2
--
--     case result of
--         Nothing      -> failure
--         Just result' -> diff (L.fromMatrix answer) (==) result'
--
--     where
--         genMatrixEntry :: Gen Int
--         genMatrixEntry = Gen.int (Range.linear 0 100000)
--
--         -- Seems can't create empty matrix with Data.Matrix
--         genMatrixSideLength :: Gen Int
--         genMatrixSideLength = Gen.int (Range.linear 1 6)
--
--         dataMatrixAdd = M.elementwise (+)
--
-- prop_matrixMultiplication :: Property
-- prop_matrixMultiplication = withTests 100 . property $ do
--     [aSize, bSize, cSize] <- forAll $ Gen.list (Range.singleton 3) genMatrixSideLength
--
--     m1 <- forAll $ genMatrix aSize bSize genMatrixEntry
--     m2 <- forAll $ genMatrix bSize cSize genMatrixEntry
--
--     let answer = m1 `M.multStd` m2
--     let result = L.multiply 0 (L.fromMatrix m1) (L.fromMatrix m2)
--
--     case result of
--         Nothing      -> failure
--         Just result' -> diff (L.fromMatrix answer) (==) result'
--
--     where
--         genMatrixEntry :: Gen Int
--         genMatrixEntry = Gen.int (Range.linear 0 100000)
--
--         -- Seems can't create empty matrix with Data.Matrix
--         genMatrixSideLength :: Gen Int
--         genMatrixSideLength = Gen.int (Range.linear 1 6)


prop_multiplicationWithDifferentDefinition0 :: Property
prop_multiplicationWithDifferentDefinition0 = unitTest $ do
    -- annotate $ (unpack $ pShow $ L.multiply zero x y)
    failure
    undefined

    where
        i :: Double
        i = read "Infinity"

        x :: (Ord a, Ord b, Num a, Num b) => L.LabelledMatrix a b TropicalSemiringValue
        x = fmap T $ fromRight $ L.fromListDefault (read "Infinity") [
              ((0, 0), 0), ((0, 1), i)
            , ((1, 0), i), ((1, 1), 0)
          ]
        y :: (Ord a, Ord b, Num a, Num b) => L.LabelledMatrix a b TropicalSemiringValue
        y = fmap T $ fromRight $ L.fromListDefault (read "Infinity") [
              ((0, 0), 0), ((0, 1), i)
            , ((1, 0), i), ((1, 1), 0)
          ]


-- prop_multiplicationWithDifferentDefinition :: Property
-- prop_multiplicationWithDifferentDefinition = unitTest $ do
--     annotate $ (unpack $ pShow $ L.multiply zero x y)
--     failure
--     undefined
--
--     where
--         i :: Double
--         i = read "Infinity"
--
--         x :: (Ord a, Ord b, Num a, Num b) => L.LabelledMatrix a b TropicalSemiringValue
--         x = fmap T $ fromRight $ L.fromListDefault (read "Infinity") [
--               ((0, 0), 0), ((0, 1), 2), ((0, 2), 10), ((0, 3), i)
--             , ((1, 0), i), ((1, 1), 0), ((1, 2), i),  ((1, 3), i)
--             , ((2, 0), i), ((2, 1), i), ((2, 2), 0),  ((2, 3), i)
--             , ((3, 0), i), ((3, 1), i), ((3, 2), i),  ((3, 3), 0)
--           ]
--         y :: (Ord a, Ord b, Num a, Num b) => L.LabelledMatrix a b TropicalSemiringValue
--         y = fmap T $ fromRight $ L.fromListDefault (read "Infinity") [
--               ((0, 0), 0), ((0, 1), i), ((0, 2), i), ((0, 3), i)
--             , ((1, 0), i), ((1, 1), 0), ((1, 2), 3), ((1, 3), i)
--             , ((2, 0), i), ((2, 1), i), ((2, 2), 0), ((2, 3), i)
--             , ((3, 0), i), ((3, 1), i), ((3, 2), i), ((3, 3), 0)
--           ]




