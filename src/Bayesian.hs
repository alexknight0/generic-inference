{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

module Bayesian
    ( queryNetwork, toProbabilityQuery
    , ProbabilityQuery
    , Probability (P)
    , Network
    , BayesValuation
    )
where

import           ShenoyShafer
import           Utils
import           ValuationAlgebra
import           ValuationAlgebra.Semiring

import           Control.DeepSeq                          (NFData)
import           Control.Exception                        (assert)
import           Data.Binary                              (Binary)
import qualified Data.Map                                 as M
import           Data.Set                                 (union)
import qualified Data.Set                                 as S
import           GHC.Generics                             (Generic)


import           Control.Distributed.Process
import           Control.Distributed.Process.Serializable

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
type BayesValuation a b = SemiringValuation Probability a b

newtype Probability = P Double deriving (Num, Fractional, Binary, Show, NFData, Ord, Eq, Generic)

instance SemiringValue Probability where
    multiply = (*)
    add = (+)
    zero = 0
    one = 1

type Network a b = [BayesValuation a b]
-- | (conditionedVariables, conditionalVariables)
type ProbabilityQuery a b = (Variables a b, Variables a b)

conditionalProbability :: (Ord a) => Variables a b -> Variables a b -> (Variables a b -> Probability) -> Probability
conditionalProbability vs givenVs p = p (unionAssertDisjoint vs givenVs) / p givenVs

queryNetwork :: forall a b. (Show a, Show b, Serializable a, Serializable b, Ord a, Ord b)
    => [ProbabilityQuery a b]
    -> Network a b
    -> Process [Probability]
queryNetwork qs network' = do
    results <- inference network' queriesForInference
    let f vs = queryToProbability vs results
    pure $ map (\(vs, givenVs) -> conditionalProbability vs givenVs f) qs

    where
        queriesForInference :: [Domain a]
        queriesForInference = map (\(vs, givenVs) -> assert (S.disjoint (M.keysSet vs) (M.keysSet givenVs)) $
                                   union (M.keysSet vs) (M.keysSet givenVs)) qs

{- | Takes a query and returns the resulting probability. Assumes the query is covered by the network. -}
queryToProbability :: (Show a, Show b, Ord a, Ord b) => Variables a b -> InferredData (SemiringValuation Probability) a b -> Probability
queryToProbability vs results = findValue vs (normalize $ answerQuery (M.keysSet vs) results)

toProbabilityQuery :: (Ord a) => ([(a, b)], [(a, b)]) -> ProbabilityQuery a b
toProbabilityQuery (x, y) = (fromListAssertDisjoint x, fromListAssertDisjoint y)

