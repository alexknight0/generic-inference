{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
--{-# LANGUAGE OverloadedLists #-}

module Bayesian
    ( getRows
    , Columns (Columns, ColumnsIdentity)
    , BayesValuation (Table)
    ,
    )
where

import ValuationAlgebra
import Data.Binary (Binary)
import GHC.Generics
import Data.Set (Set, member, fromList, toList, empty, intersection, union)
import qualified Data.Set as S
import Utils (setMap, nubWithBy)
import Debug.Trace
import Control.Exception (assert)

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
    label (Table (x : _)) = setMap fst (variables x)

    -- Identity / neutral element must be addressed here, or a plan made to address it in the main typeclass.
    combine Identity x = x
    combine x Identity = x
    combine (Table []) _ = Table []
    combine _ (Table []) = Table []
    combine (Table (x:xs)) (Table (y:ys)) = Table $
            [Row (union (variables a) (variables b)) (probability a * probability b)
                | a <- (x:xs), b <- (y:ys), sharedVariablesAreSameValue numSharedVars a b]

        where
            numSharedVars = fromIntegral . length $ intersection (setMap fst (variables x)) (setMap fst (variables y))

    -- todo can upgrade to hashmap.
    project Identity _ = Identity
    project (Table xs) domain =  Table $ nubWithBy (\(Row vs _) -> vs) addRows $ map (\(Row vs p) -> Row (projectVars vs) p) xs
        where
            projectVars = S.filter (\x -> fst x `elem` domain)
            addRows (Row vs1 p1) (Row vs2 p2) = assert (vs1 == vs2) $ Row vs1 (p1 + p2)

    identity = Identity

instance (Show a, Ord a) => Show (BayesValuation a b) where
    show = show . getColumns

-- An inefficent storage format, but we should get a working implementation first.
data Row a b = Row
    {
        variables :: Set (Variable a b),
        probability :: Probability
    }
    deriving (Show, Generic, Binary)

type Variable a b = (a, b)

-- A supporting data structure, as inputting data in this format is often easier.
data Columns a b = Columns (Set a) [Probability] | ColumnsIdentity deriving (Show)

getColumns :: (Ord a) => BayesValuation a b -> Columns a b
getColumns Identity = ColumnsIdentity
getColumns (Table []) = Columns empty []
getColumns (Table rs'@(r : _)) = Columns vs ps
    where
        vs = setMap fst (variables r)
        ps = map probability rs'

getRows :: forall a b. (Enum b, Bounded b, Ord a, Ord b) => Columns a b -> BayesValuation a b
getRows ColumnsIdentity = Identity
getRows (Columns vars ps) = Table $ zipWith Row (vPermutations (toList vars)) ps
    where
        vPermutations :: [a] -> [Set (Variable a b)]
        vPermutations [] = [empty]
        vPermutations (v : vs) = fmap fromList [(v, vVal) : rest | vVal <- varValues, rest <- map toList $ vPermutations vs]

        varValues :: [b]
        varValues = [minBound .. maxBound]

type Probability = Float

-- Returns true iff the two rows should be combined as a part of a combine operation.
-- The rows should be combined if all of their shared variables are the same value.
sharedVariablesAreSameValue :: (Ord a, Ord b) => Integer -> Row a b -> Row a b -> Bool
sharedVariablesAreSameValue numSharedVariables x y =
        fromIntegral (length (intersection (variables x) (variables y))) == numSharedVariables


