{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

module LocalComputation.Instances.BayesianNetwork
    ( getProbability, toProbabilityQuery
    , ProbabilityQuery
    , Probability (P)
    , Network
    , BayesianNetworkValuation
    )
where

import           LocalComputation.Inference.ShenoyShafer
import           LocalComputation.Utils
import           LocalComputation.ValuationAlgebra
import           LocalComputation.ValuationAlgebra.Semiring

import           Control.DeepSeq                            (NFData)
import           Control.Exception                          (assert)
import           Data.Binary                                (Binary)
import qualified Data.Hashable                              as H
import qualified Data.Map                                   as M
import           Data.Set                                   (union)
import qualified Data.Set                                   as S
import           GHC.Generics                               (Generic)


import           Control.Distributed.Process
import           Control.Distributed.Process.Serializable
import           Data.List.Extra                            (chunksOf)
import qualified LocalComputation.Inference                 as I

{- | Valuation for the valuation algebra of probability potentials.

While the initial valuations before inference are entered as conditional probability tables
as shown below, after inference these valuations have looser meaning, and should be thought
of as simply unnormalized probability distributions.

    var   A   B   Probability

     0    0   0       0.7       P(var == 0 | A == 0 && B == 0)

     0    0   1       0.4       P(var == 0 | A == 0 && B == 1)

     0    0   2       0.3       P(var == 0 | A == 0 && B == 2)

     0    1   0       0.6       P(var == 0 | A == 1 && B == 0)

     .    .   .        .                      .
     .    .   .        .                      .
     .    .   .        .                      .
-}
type BayesianNetworkValuation a b = SemiringValuation Probability a b

newtype Probability = P Double deriving (Num, Fractional, Binary, Show, NFData, Ord, Eq, Generic)

instance SemiringValue Probability where
    add = (+)
    multiply = (*)
    zero = 0
    one = 1

type Network a b = [BayesianNetworkValuation a b]

-- | (conditionedVariables, conditionalVariables)
type ProbabilityQuery a b = (VariableArrangement a b, VariableArrangement a b)

conditionalProbability :: (Ord a) => VariableArrangement a b -> VariableArrangement a b -> (VariableArrangement a b -> Probability) -> Probability
conditionalProbability vars givenVars p = p (unionAssertDisjoint vars givenVars) / p givenVars

-- TODO: I thought we had to normalize, but this is passing all tests?

-- | Returns the probability of a given event occuring, given a set of conditional variables.
-- Takes a network (a list of conditional probability tables) as input.
-- getProbability :: forall a b. (H.Hashable a, H.Hashable b, Show a, Show b, Serializable a, Serializable b, Ord a, Ord b, NFData a, NFData b)
--     => [ProbabilityQuery a b]
--     -> Network a b
--     -> Process [Probability]
-- getProbability qs network' = do
--     results <- fromRight $ I.queries I.Shenoy network' domains
--     pure $ zipWith (\(vars, givenVars) [top, bottom] -> findValue (unionAssertDisjoint vars givenVars) top / findValue givenVars bottom) qs (chunksOf 2 results)
--     -- let f vars = queryToProbability vars results
--     -- pure $ map (\(vars, givenVars) -> conditionalProbability vars givenVars f) qs
--
--     where
--         -- The two queries we need
--         domains :: [Domain a]
--         domains = concatMap (\(vs, givenVs) -> [unionAssertDisjoint' (M.keysSet vs) (M.keysSet givenVs)
--                                               , M.keysSet givenVs
--                                              ]) qs

{- | Takes a query and returns the resulting probability. Assumes the query is covered by the network. -}
queryToProbability :: (Show a, Show b, Ord a, Ord b) => VariableArrangement a b -> InferredData (SemiringValuation Probability) a b -> Probability
queryToProbability vars results = findValue vars (normalize $ answerQuery (M.keysSet vars) results)

toProbabilityQuery :: (Ord a) => ([(a, b)], [(a, b)]) -> ProbabilityQuery a b
toProbabilityQuery (x, y) = (fromListAssertDisjoint x, fromListAssertDisjoint y)



getProbability :: forall a b. (H.Hashable a, H.Hashable b, Show a, Show b, Serializable a, Serializable b, Ord a, Ord b)
    => [ProbabilityQuery a b]
    -> Network a b
    -> Process [Probability]
getProbability qs network' = do
    results <- inference network' queriesForInference
    let f vs = queryToProbability vs results
    pure $ map (\(vs, givenVs) -> conditionalProbability vs givenVs f) qs

    where
        queriesForInference :: [Domain a]
        queriesForInference = map (\(vs, givenVs) -> assert (S.disjoint (M.keysSet vs) (M.keysSet givenVs))
                                                            (union (M.keysSet vs) (M.keysSet givenVs))
                                    ) qs

