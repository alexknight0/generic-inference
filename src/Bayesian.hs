{-# LANGUAGE ScopedTypeVariables #-}

module Bayesian
    ()
where

import ValuationAlgebra

-- Don't be suprised if you need to put (Enum, bounded) on 'b'.
instance Valuation (BayesValuation varVal) where
    -- label :: BVal varVal var -> Domain var
    label (BValRows []) = []
    label (BValRows (x : _)) = fst (variable x) : map fst (conditions x)

    -- combine :: BVal varVal var -> BVal varVal var -> BVal varVal var
    combine = undefined

    -- There is a lot about the data format i'm unsure about here -
    -- what if we get a p1 = A | B C and p2 = B | C scenario? Can this happen?

    -- project :: BVal varVal var -> Domain var -> BVal varVal var
    project = undefined

instance (Show b) => Show (BayesValuation a b) where
    show xs = show $ getColumns xs


---------------- BAYESIAN

-- This implementation only handles binary events.

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
data BayesValuation var val = BValRows [Row val var]

-- An inefficent storage format, but we should get a working implementation first.
data Row a b = BValRow
    { variable :: (a, b),
        conditions :: [(a, b)],
        probability :: Probability
    }
    deriving (Show)

-- A supporting data structure, as inputting data in this format is often easier.
data Columns a b = Columns a [a] [Probability] | ColumnsNull deriving (Show)

getColumns :: BayesValuation var val -> Columns val var
getColumns (BValRows []) = ColumnsNull
getColumns (BValRows rs'@(r : _)) = Columns v cs ps
    where
        v = fst (variable r)
        cs = map (fst) (conditions r)
        ps = map probability rs'

getRows :: forall val var. (Enum var, Bounded var) => Columns val var -> BayesValuation var val
getRows ColumnsNull = BValRows []
getRows (Columns var conds ps) = BValRows fullRows'
    where
        fullRows' :: [Row val var]
        fullRows' = map (\(x, y, z) -> BValRow x y z) fullRows

        fullRows :: [((val, var), [(val, var)], Probability)]
        fullRows = zipWith (\(v, cs) p -> (v, cs, p)) rowsWithoutProbability ps

        rowsWithoutProbability :: [((val, var), [(val, var)])]
        rowsWithoutProbability = [(v, cs) | v <- vPermutations, cs <- csPermutations conds]
            where
                vPermutations :: [(val, var)]
                vPermutations = [(var, vVal) | vVal <- varValues]

                csPermutations :: [val] -> [[(val, var)]]
                csPermutations [] = [[]]
                csPermutations (c : cs) = [(c, cVal) : rest | cVal <- varValues, rest <- csPermutations cs]

                varValues :: [var]
                varValues = [minBound .. maxBound]

type Probability = Float

---------------- BAYESIAN END

---------------- BAYESIAN PROBLEM EXAMPLE
data P1Var = F | B | L | D | H deriving (Eq, Ord, Show)

data P1Value = P1False | P1True deriving (Enum, Bounded, Show)

p1Valuations :: [BayesValuation P1Value P1Var]
p1Valuations =
    [ getRows $ Columns F [] [0.85, 0.15],
        getRows $ Columns B [] [0.99, 0.01],
        getRows $ Columns L [F] [0.95, 0.4, 0.05, 0.6],
        getRows $ Columns D [F, B] [0.7, 0.03, 0.1, 0.01, 0.3, 0.97, 0.9, 0.99],
        getRows $ Columns H [D] [0.99, 0.3, 0.01, 0.7]
    ]

-- BAYESIAN PROBLEM EXAMPLE END
