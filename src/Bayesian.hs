{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
--{-# LANGUAGE OverloadedLists #-}

module Bayesian
    ( getRows, showAsRows, normalize, queryNetwork
    , Columns (Columns, ColumnsIdentity)
    , BayesValuation (Table)
    , ProbabilityQuery
    )
where

import ValuationAlgebra
import ShenoyShafer
import Utils

import Data.Binary (Binary)
import GHC.Generics
import Data.Set (Set, member, fromList, toList, empty, intersection, union)
import qualified Data.Set as S
import Utils (setMap, nubWithBy)
import Debug.Trace
import Control.Exception (assert)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S


import Control.Distributed.Process
import Control.Distributed.Process.Serializable

{-
In BValColumns, the first parameter is the variable, the second parameter
is a list of the conditions, and the third parameter is the list of probabilities,
where probabilities are ordered as follows:

    var   A   B   Probability

     0    0   0       0.7       P(var == 0 | A == 0 && B == 0)

     0    0   1       0.4       P(var == 0 | A == 0 && B == 1)

     0    0   2       0.3       P(var == 0 | A == 0 && B == 2)

     0    1   0       0.6       P(var == 0 | A == 1 && B == 0)

     .    .   .        .                      .
     .    .   .        .                      .
     .    .   .        .                      .

BValRows stores the equivalent information except as what is essentially as a tuple of each
row of the table instead.

Note that neither form allows the range of values that can be stored in a certain field differ across fields;
i.e. if 'A' can range from 0 to 1, then B must also range from 0 to 1.

BValColumns stores no redundant information, while BValRows stores a heap of redundant information,
but allows accessing this information in a more haskell-like manner.
-}
data BayesValuation a b = Table [Row a b] | Identity deriving (Generic, Binary)

-- Don't be suprised if you need to put (Enum, bounded) on 'b'.
instance Valuation BayesValuation where
    label Identity = empty
    label (Table []) = empty
    label (Table (x : _)) = M.keysSet (variables x)

    -- Identity / neutral element must be addressed here, or a plan made to address it in the main typeclass.
    combine Identity x = x
    combine x Identity = x
    combine (Table []) _ = Table []
    combine _ (Table []) = Table []
    combine (Table (x:xs)) (Table (y:ys)) = Table $
            [Row (unionUnsafe (variables a) (variables b)) (probability a * probability b)
                | a <- (x:xs), b <- (y:ys), sharedVariablesAreSameValue numSharedVars a b]

        where
            numSharedVars = fromIntegral . length $ intersection (M.keysSet (variables x)) (M.keysSet (variables y))

    -- todo can upgrade to hashmap.
    project Identity _ = Identity
    project (Table xs) domain = Table $ nubWithBy (\(Row vs _) -> vs) addRows $ map (\(Row vs p) -> Row (projectedDomain vs) p) xs
        where
            projectedDomain = M.filterWithKey (\k _ -> k `elem` domain)
            addRows (Row vs1 p1) (Row vs2 p2) = assert (vs1 == vs2) $ Row vs1 (p1 + p2)

    identity = Identity

instance (Show a, Show b) => Show (BayesValuation a b) where
    show = showAsRows

showAsRows :: (Show a, Show b) => BayesValuation a b -> String
showAsRows (Table xs) = "------ Table ------\n"
                     ++ concatMap (\(Row vs p) -> show vs ++ "   " ++ show p ++ "\n") xs
                     ++ "-------------------\n"
showAsRows Identity = "------ Table ------\n"
                   ++ "Identity"
                   ++ "-------------------\n"


-- An inefficent storage format, but we should get a working implementation first.
data Row a b = Row
    {
        variables :: Variables a b,
        probability :: Probability
    }
    deriving (Show, Generic, Binary)

type Variable a b = (a, b)
type Variables a b = M.Map a b

-- A supporting data structure, as inputting data in this format is often easier.
data Columns a b = Columns [a] [Probability] | ColumnsIdentity deriving (Show)

-- getColumns :: (Ord a) => BayesValuation a b -> Columns a b
-- getColumns Identity = ColumnsIdentity
-- getColumns (Table []) = Columns empty []
-- getColumns (Table rs'@(r : _)) = Columns vs ps
--     where
--         vs = setMap fst (variables r)
--         ps = map probability rs'

getRows :: forall a b. (Enum b, Bounded b, Ord a) => Columns a b -> BayesValuation a b
getRows ColumnsIdentity = Identity
getRows (Columns vars ps) = Table $ zipWith Row (vPermutations vars) ps
    where
        varValues :: [b]
        varValues = [minBound .. maxBound]

        vPermutations :: [a] -> [Variables a b]
        vPermutations xs = map fromListAssertDisjoint $ vPermutations' xs
            where
                vPermutations' :: [a] -> [[Variable a b]]
                vPermutations' [] = [[]]
                vPermutations' (v : vs) = [(v, vVal) : rest | vVal <- varValues, rest <- vPermutations' vs]


normalize :: BayesValuation a b -> BayesValuation a b
normalize Identity = Identity
normalize (Table xs) = Table $ fmap (\(Row vs p) -> Row vs (p / sumOfAllPs)) xs
    where
        sumOfAllPs = sum $ map (\(Row _ p) -> p) xs

type Probability = Double
type Network a b = [BayesValuation a b]
type ProbabilityQuery a b = (M.Map a b, M.Map a b)

-- Returns true iff the two rows should be combined as a part of a combine operation.
-- The rows should be combined if all of their shared variables are the same value.
sharedVariablesAreSameValue :: (Ord a, Ord b) => Integer -> Row a b -> Row a b -> Bool
sharedVariablesAreSameValue numSharedVariables x y =
        fromIntegral (length (intersection (S.fromList $ M.assocs $ variables x) (S.fromList $ M.assocs $ variables y))) == numSharedVariables

conditionalProbability :: (Ord a) => Variables a b -> Variables a b -> (Variables a b -> Probability) -> Probability
conditionalProbability vs givenVs p = p (unionAssertDisjoint vs givenVs) / p givenVs

-- unsafe
findProbability :: (Eq a, Eq b) => Variables a b -> BayesValuation a b -> Probability
findProbability x (Table rows) = (\(Row _ p) -> p) $ findAssertSingleMatch (\(Row vs _) -> vs == x) rows
findProbability _ Identity = error "findProbability: Attempted to read probability from an identity valuation."

queryNetwork :: forall a b. (Serializable a, Serializable b, Ord a, Ord b)
    => [ProbabilityQuery a b]
    -> Network a b
    -> Process [Probability]
queryNetwork qs network = do
    results <- inference network queriesForInference
    let f vs = queryToProbability vs results
    pure $ map (\(vs, givenVs) -> conditionalProbability vs givenVs f) qs

    where
        queriesForInference :: [Domain a]
        queriesForInference = map (\(vs, givenVs) -> assert (S.disjoint (M.keysSet vs) (M.keysSet givenVs)) $
                                   union (M.keysSet vs) (M.keysSet givenVs)) qs





--- Probably wnat an interface such that we don't rely on shenoyshafer exporting a type (Domain a, BayesValuation a) as that seems a bit internal....
-- try only use shenoyINfernece.

-- ASSUMES THE QUERY IS COVERED BY THE NETWORK
queryToProbability :: (Ord a, Ord b) => Variables a b -> InferredData BayesValuation a b -> Probability
queryToProbability vs results = findProbability vs (normalize $ answerQuery (M.keysSet vs) results)

-- queryToProbability :: ProbabilityQuery a b -> InferredData v a b -> Probability
-- queryToProbability (vs, givenVs) results = findProbability 
--     where
--         numeratorDomain = assert (S.disjoint (M.keysSet vs) (M.keysSet givenVs)) $
--                               union (M.keysSet vs) (M.keysSet givenVs)
--         denominatorDomain = M.keysSet givenVs
-- 
--         numeratorVs
-- 
--         numeratorTable = normalize $ answerQuery numeratorDomain results
--         denominatorTable = normalize $ answerQuery denominatorDomain results






            




        
        










