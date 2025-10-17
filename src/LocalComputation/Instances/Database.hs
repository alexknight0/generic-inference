{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
module LocalComputation.Instances.Database (

) where

import           Control.Monad.IO.Class                      (MonadIO)
import           Data.Functor                                (void)
import qualified Data.Set                                    as S
import qualified LocalComputation.Inference                  as I
import qualified LocalComputation.Inference.JoinTree.Diagram as D
import qualified LocalComputation.Inference.MessagePassing   as MP
import qualified LocalComputation.Pretty                     as P
import qualified LocalComputation.ValuationAlgebra           as V

--------------------------------------------------------------------------------
-- Datatype definition
--------------------------------------------------------------------------------
data Table a b = Identity { domain :: S.Set b } | Table { headings :: [b], rows :: [[a]] } deriving (V.Generic, V.Binary, V.NFData)

type Database a b = [Table a b]

--------------------------------------------------------------------------------
-- Example problem
--------------------------------------------------------------------------------
table1 :: Table String String
table1 = Table {
      headings =  ["Name",  "Age",  "Birthplace"]
    , rows     = [
                  ["Ben",   "25",   "China"]
                , ["Julia", "28",   "Australia"]
                ]


}

table2 :: Table String String
table2 = Table {
      headings =  ["Name",  "ID"]
    , rows     = [
                  ["Ben",   "1"]
                , ["David", "2"]
                , ["Julia", "3"]
               ]
}

query :: S.Set String
query = S.fromList $ ["ID", "Age"]

--------------------------------------------------------------------------------
-- Instance definition
--------------------------------------------------------------------------------


instance (Table a b ~ Table String String) => V.ValuationFamily (Table a) where

    label t = S.fromList $ t.headings

    -- TODO: Think we need to redo the identity thing; vaccuous extension is sooo distracting.
    _combine Identity{} t = t
    _combine _ _          = undefined
    _combine t1 t2        = innerJoinTables t1 t2

    _project Identity{} newDomain = Identity newDomain
    _project t newDomain = Table (filter (`elem` newDomain) t.headings) newRows
        where
            newHeadings = filter (`elem` newDomain) t.headings
            newRows = map f t.rows
                where
                    f row = [value | (heading, value) <- zip t.headings row, heading `elem` newDomain]

    identity t = Table [] [[]]


innerJoinTables :: (Eq a, Eq b) => Table a b -> Table a b -> Table a b
innerJoinTables t1 t2 = Table headings [map snd $ mergeRows t1row' t2row' | t1row <- t1.rows, t2row <- t2.rows,
                                                            let t1row' = zip t1.headings t1row,
                                                            let t2row' = zip t2.headings t2row,
                                                            numMatchingVariables t1row' t2row' == expectedMatchingVariables]
    where
        headings = t1.headings ++ filter (`notElem` t1.headings) t2.headings
        expectedMatchingVariables = length $ filter (`elem` t2.headings) t1.headings

mergeRows :: (Eq a) => [(a, b)] -> [(a, b)] -> [(a, b)]
mergeRows row otherRow = row ++ (filter (\(x, _) -> x `notElem` variablesAlreadyPresent) otherRow)
    where
        variablesAlreadyPresent = map fst row

numMatchingVariables :: (Eq a, Eq b) => [(a, b)] -> [(a, b)] -> Int
numMatchingVariables row otherRow = sum $ map (\entry -> case entry `elem` otherRow of
                                                                    True  -> 1
                                                                    False -> 0) row



--------------------------------------------------------------------------------
-- Answering
--------------------------------------------------------------------------------

answer :: (MonadIO m) => m (Table String String)
answer = fmap (.c) $ I.unsafeQuery I.Fusion [table1, table2] query

drawGraph :: (MonadIO m) => m ()
drawGraph = void $ I.unsafeQueryDrawGraph draw (I.Shenoy MP.Threads) [table1, table2] query
    where
        draw = D.def { D.beforeInference = Just "diagrams/demo/before-inference.svg"
                     , D.afterInference = Just "diagrams/demo/after-inference.svg"
                    }


{-

>>> drawGraph
/home/alex/localcomputation/src/LocalComputation/Instances/Database.hs:58:10-73: No instance nor default method for class operation identity

1   25
3   28

-}

--------------------------------------------------------------------------------
-- Definitions
--------------------------------------------------------------------------------
{- | How do we represent this table as a valuation?

Variables = String



-}








foobar = True


--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------
instance Show (Table String String) where
    show t = P.showTable $ P.unsafeTable t.headings t.rows



