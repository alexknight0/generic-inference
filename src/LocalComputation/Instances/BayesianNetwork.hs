{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

module LocalComputation.Instances.BayesianNetwork
    ( getProbability, toQuery
    , Query (conditioned, conditional)
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

-- TODO: See if can handle conditioned being in conditional and vice versa - will allow us to eliminate
-- 'disjoint union' checks.

-- | A query for a bayesian network is a query for the conditional probability of some event.
-- In other words, a query for \(P(\text{conditioned} | \text{conditional})\) where
-- 'conditioned' and 'conditional' are sets of variables.
data Query a b = Query {
      conditioned :: VariableArrangement a b
    , conditional :: VariableArrangement a b
} deriving (Show)

conditionalProbability :: (Ord a) => Query a b -> (VariableArrangement a b -> Probability) -> Probability
conditionalProbability q p = p (unionAssertDisjoint q.conditioned q.conditional) / p q.conditional

-- TODO: I thought we had to normalize, but this is passing all tests?
-- TODO: Below uses more generic inference, but appears to perform slower?

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

toQuery :: (Ord a) => ([(a, b)], [(a, b)]) -> Query a b
toQuery (x, y) = Query (fromListAssertDisjoint x) (fromListAssertDisjoint y)

getProbability :: forall a b. (H.Hashable a, H.Hashable b, Show a, Show b, Serializable a, Serializable b, Ord a, Ord b)
    => [Query a b]
    -> Network a b
    -> Process [Probability]
getProbability qs network' = do
    results <- inference network' queriesForInference
    let f vs = queryToProbability vs results
    pure $ map (\q -> conditionalProbability q f) qs

    where
        queriesForInference :: [Domain a]
        queriesForInference = map (\q -> assert (S.disjoint (M.keysSet q.conditioned) (M.keysSet q.conditional))
                                                            (union (M.keysSet q.conditioned) (M.keysSet q.conditional))
                                    ) qs

