{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Bayesian
    ( getRows
    , Columns (Columns, ColumnsIdentity)
    , BayesValuation
    )
where

import ValuationAlgebra
import Data.Binary (Binary)
import GHC.Generics

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
    label Identity = []
    label (Table []) = []
    label (Table (x : _)) = map fst (variables x)

    -- Identity / neutral element must be addressed here, or a plan made to address it in the main typeclass.
    combine Identity _ = Identity
    combine _ Identity = Identity
    combine _ _ = undefined

    -- There is a lot about the data format i'm unsure about here -
    -- what if we get a p1 = A | B C and p2 = B | C scenario? Can this happen?
    project Identity _ = Identity
    project _ _ = undefined

    identity = Identity

instance (Show a) => Show (BayesValuation a b) where
    show = show . getColumns

-- An inefficent storage format, but we should get a working implementation first.
data Row a b = Row
    {
        variables :: [Variable a b],
        probability :: Probability
    }
    deriving (Show, Generic, Binary)

type Variable a b = (a, b)

-- A supporting data structure, as inputting data in this format is often easier.
data Columns a b = Columns [a] [Probability] | ColumnsIdentity deriving (Show)

getColumns :: BayesValuation a b -> Columns a b
getColumns Identity = ColumnsIdentity
getColumns (Table []) = Columns [] []
getColumns (Table rs'@(r : _)) = Columns vs ps
    where
        vs = map fst (variables r)
        ps = map probability rs'

getRows :: forall a b. (Enum b, Bounded b) => Columns a b -> BayesValuation a b
getRows ColumnsIdentity = Identity
getRows (Columns vars ps) = Table $ zipWith Row (vPermutations vars) ps
    where
        vPermutations :: [a] -> [[Variable a b]]
        vPermutations [] = [[]]
        vPermutations (v : vs) = [(v, vVal) : rest | vVal <- varValues, rest <- vPermutations vs]

        varValues :: [b]
        varValues = [minBound .. maxBound]

type Probability = Float

