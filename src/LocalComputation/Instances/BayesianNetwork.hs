{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

module LocalComputation.Instances.BayesianNetwork
    ( getProbability, toQuery
    , getProbabilityAlt
    , Query (conditioned, conditional)
    , Probability (P)
    , Network
    , Valuation
    )
where

import           LocalComputation.Inference.ShenoyShafer
import           LocalComputation.Utils
import qualified LocalComputation.ValuationAlgebra          as V
import qualified LocalComputation.ValuationAlgebra.Semiring as S

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
type Valuation a b = S.SemiringValuation Probability b a
type VarAssignment a b = V.VarAssignment (S.SemiringValuation Probability b) a b

newtype Probability = P Double deriving (Num, Fractional, Binary, Show, NFData, Ord, Eq, Generic)

instance S.SemiringValue Probability where
    add = (+)
    multiply = (*)
    zero = 0
    one = 1

type Network a b = [Valuation a b]

-- TODO: See if can handle conditioned being in conditional and vice versa - will allow us to eliminate
-- 'disjoint union' checks.

-- | A query for a bayesian network is a query for the conditional probability of some event.
-- In other words, a query for \(P(\text{conditioned} | \text{conditional})\) where
-- 'conditioned' and 'conditional' are sets of variables.
data Query a b = Query {
      conditioned :: VarAssignment a b
    , conditional :: VarAssignment a b
} deriving (Show)

threshold :: Probability
threshold = 1 * 10 ^^ (-10 :: Integer)

approxEqual :: Probability -> Probability -> Bool
approxEqual x y = abs (x - y) < threshold

-- | Returns the conditional probability of a certain event.
conditionalP :: (Ord a, Ord b) => Query a b -> (VarAssignment a b -> Probability) -> Probability
conditionalP q p
    | not $ sharedKeysHaveSameValue q.conditioned q.conditional = 0
    | p q.conditional `approxEqual` 0                           = 0
    | otherwise                                                 =   p (M.union q.conditioned q.conditional)
                                                                  / p q.conditional

sharedKeysHaveSameValue :: (Ord a, Ord b) => M.Map a b -> M.Map a b -> Bool
sharedKeysHaveSameValue m1 m2 = allTrue $ M.intersectionWith f (withIndicator m1) (withIndicator m2)
    where
        withIndicator = M.map (\x -> (x, True))

        -- Set indicator to false if values not equal
        f (x, False) _          = (x, False)
        f _          (x, False) = (x, False)
        f (x, _)     (y, _)     = (x, x == y)

        allTrue = all ((== True) . snd . snd) . M.toList

-- TODO: I thought we had to normalize, but this is passing all tests?
-- TODO: Below uses more generic inference, but appears to perform slower?

-- | Returns the probability of a given event occuring, given a set of conditional variables.
-- Takes a network (a list of conditional probability tables) as input.
getProbabilityAlt :: forall a b. ( Show a, Show b, Serializable a, Serializable b, Ord a, Ord b, NFData a, NFData b)
    =>[Query a b]
    -> Network a b
    -> Process [Probability]
getProbabilityAlt qs network' = do
    results <- fromRight $ I.queries I.Shenoy network' domains
    pure $ zipWith (\q [top, bottom] -> S.findValue (unionAssertDisjoint q.conditioned q.conditional) top / S.findValue q.conditional bottom) qs (chunksOf 2 results)
    -- let f vars = queryToProbability vars results
    -- pure $ map (\(vars, givenVars) -> conditionalProbability vars givenVars f) qs

    where
        -- The two queries we need
        domains :: [V.Domain a]
        domains = concatMap (\q -> [union (M.keysSet q.conditioned) (M.keysSet q.conditional)
                                  , M.keysSet q.conditional
                                             ]) qs

{- | Takes a query and returns the resulting probability. Assumes the query is covered by the network. -}
queryToProbability :: (Ord a, Show a, Ord b, Show b) => VarAssignment a b -> InferredData (S.SemiringValuation Probability b) a -> Probability
queryToProbability vars results = S.findValue vars (S.normalize $ answerQuery (M.keysSet vars) results)

toQuery :: (Ord a) => ([(a, b)], [(a, b)]) -> Query a b
toQuery (x, y) = Query (fromListAssertDisjoint x) (fromListAssertDisjoint y)

getProbability :: forall a b. (Show a, Show b, Serializable a, Serializable b, Ord a, Ord b)
    => [Query a b]
    -> Network a b
    -> Process [Probability]
getProbability qs network' = do
    results <- inference network' queriesForInference
    let f vs = queryToProbability vs results
    pure $ map (\q -> conditionalP q f) qs

    where
        queriesForInference :: [V.Domain a]
        queriesForInference = map (\q -> union (M.keysSet q.conditioned) (M.keysSet q.conditional)) qs

