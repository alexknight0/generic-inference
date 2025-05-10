{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Bayesian
    ( getRows
    , Columns (Columns, ColumnsIdentity, ColumnsNull)
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

BValRows stores the equivalent information in a different format, storing each
row of the table instead.

BValColumns stores no redundant information, while BValRows stores a heap of redundant information,
but allows accessing this information in a more haskell-like manner.
-}
data BayesValuation a b = Rows [Row a b] | Identity deriving (Generic, Binary)

-- Don't be suprised if you need to put (Enum, bounded) on 'b'.
instance Valuation BayesValuation where
    label (Rows []) = []
    label (Rows (x : _)) = fst (variable x) : map fst (conditions x)
    label Identity = []

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
    { variable :: (a, b),
        conditions :: [(a, b)],
        probability :: Probability
    }
    deriving (Show, Generic, Binary)

-- A supporting data structure, as inputting data in this format is often easier.
data Columns a b = Columns a [a] [Probability] | ColumnsNull | ColumnsIdentity deriving (Show)

getColumns :: BayesValuation a b -> Columns a b
getColumns Identity = ColumnsIdentity
getColumns (Rows []) = ColumnsNull
getColumns (Rows rs'@(r : _)) = Columns v cs ps
    where
        v = fst (variable r)
        cs = map (fst) (conditions r)
        ps = map probability rs'

getRows :: forall a b. (Enum b, Bounded b) => Columns a b -> BayesValuation a b
getRows ColumnsIdentity = Identity
getRows ColumnsNull = Rows []
getRows (Columns var conds ps) = Rows fullRows'
    where
        fullRows' :: [Row a b]
        fullRows' = map (\(x, y, z) -> Row x y z) fullRows

        fullRows :: [((a, b), [(a, b)], Probability)]
        fullRows = zipWith (\(v, cs) p -> (v, cs, p)) rowsWithoutProbability ps

        rowsWithoutProbability :: [((a, b), [(a, b)])]
        rowsWithoutProbability = [(v, cs) | v <- vPermutations, cs <- csPermutations conds]
            where
                vPermutations :: [(a, b)]
                vPermutations = [(var, vVal) | vVal <- varValues]

                csPermutations :: [a] -> [[(a, b)]]
                csPermutations [] = [[]]
                csPermutations (c : cs) = [(c, cVal) : rest | cVal <- varValues, rest <- csPermutations cs]

                varValues :: [b]
                varValues = [minBound .. maxBound]

type Probability = Float

