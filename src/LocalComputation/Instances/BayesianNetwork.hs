{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

module LocalComputation.Instances.BayesianNetwork
    ( getProbability, toQueryA
    , Query (conditioned, conditional, Query)
    , Probability (P)
    , Network
    , Valuation
    , VarAssignment
    , toInferenceQuery
    )
where

import           LocalComputation.Utils
import qualified LocalComputation.Utils                     as M (fromListA)
import qualified LocalComputation.ValuationAlgebra          as V
import qualified LocalComputation.ValuationAlgebra.Semiring as S

import           Control.DeepSeq                            (NFData)
import           Data.Binary                                (Binary)
import qualified Data.Map                                   as M
import           Data.Set                                   (union)
import           GHC.Generics                               (Generic)


import           Control.Distributed.Process
import           Control.Distributed.Process.Serializable
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
type Valuation a b = S.Valuation Probability b a
type VarAssignment a b = V.VarAssignment (S.Valuation Probability b) a b

newtype Probability = P Double deriving (Num, Fractional, Binary, Show, NFData, Ord, Eq, Generic)

instance S.SemiringValue Probability where
    add = (+)
    multiply = (*)
    zero = 0
    one = 1

type Network a b = [Valuation a b]

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
conditionalP :: (Ord a, Ord b) => Query a b -> (VarAssignment a b -> Probability) -> (VarAssignment a b -> Probability) -> Probability
conditionalP q p1 p2
    | not $ sharedKeysHaveSameValue q.conditioned q.conditional = 0
    | p2 q.conditional `approxEqual` 0                          = 0
    | otherwise                                                 = p1 (M.union q.conditioned q.conditional)
                                                                / p2 q.conditional

-- TODO: Wait isn't this just M.intersectionWith const m1 m2  == M.intersectionWith (flip const) m1 m2
-- Edit: Simply replace with whatever the efficent way is of doing this, as we will find out when optimizing semiring.
sharedKeysHaveSameValue :: (Ord a, Ord b) => M.Map a b -> M.Map a b -> Bool
sharedKeysHaveSameValue m1 m2 = allTrue $ M.intersectionWith f (withIndicator m1) (withIndicator m2)
    where
        withIndicator = M.map (\x -> (x, True))

        -- Set indicator to false if values not equal
        f (x, False) _          = (x, False)
        f _          (x, False) = (x, False)
        f (x, _)     (y, _)     = (x, x == y)

        allTrue = all ((== True) . snd . snd) . M.toList

-- | Returns the probability of a given event occuring, given a set of conditional variables.
-- Takes a network (a list of conditional probability tables) as input.
getProbability :: forall a b. (Show a, Show b, Serializable a, Serializable b, Ord a, Ord b, NFData a, NFData b)
    => [Query a b]
    -> Network a b
    -> Process [Probability]
getProbability qs network' = do
    results <- fromRight $ I.queries I.Shenoy network' domains
    pure $ zipWith probability qs results

    where
        domains :: [V.Domain a]
        domains = map (\q -> union (M.keysSet q.conditioned) (M.keysSet q.conditional)) qs

        probability q r = conditionalP q (\x -> S.findValue x r)
                                         (\x -> S.findValue x (V.project r (M.keysSet q.conditional)))

-- TODO: Unsafe don't export.
toQueryA :: (Ord a) => ([(a, b)], [(a, b)]) -> Query a b
toQueryA (x, y) = Query (M.fromListA x) (M.fromListA y)

-- | Converts a given probability query to an inference query. This inference query will allow inference
-- to return the results necessary to answer the given probability query.
toInferenceQuery :: (Ord a) => [Query a b] -> [V.Domain a]
toInferenceQuery qs = map (\q -> union (M.keysSet q.conditioned) (M.keysSet q.conditional)) qs

