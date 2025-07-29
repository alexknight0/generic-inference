{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LocalComputation.Instances.ShortestPath.SingleTarget
    (
      singleTarget
    , singleTarget'
    , InvalidGraph (MissingZeroCostSelfLoops)
    )
where

import           Control.Distributed.Process                                  (Process)
import           Data.Maybe                                                   (fromJust)
import qualified Data.Set                                                     as S

import qualified Data.Map                                                     as MP
import           LocalComputation.Inference.ShenoyShafer                      (answerQueriesM,
                                                                               answerQueryM)
import qualified LocalComputation.LabelledMatrix                              as M
import qualified LocalComputation.ValuationAlgebra.QuasiRegular               as Q (QuasiRegularValuation,
                                                                                    create,
                                                                                    solution)
import           LocalComputation.ValuationAlgebra.QuasiRegular.SemiringValue

-- Typeclasses
import           Control.DeepSeq                                              (NFData)
import           Data.Binary                                                  (Binary)
import qualified Data.Hashable                                                as H
import           Debug.Pretty.Simple                                          (pTraceShow,
                                                                               pTraceShowId)
import           GHC.Generics                                                 (Generic)
import           LocalComputation.Graph                                       as G
import           LocalComputation.Inference.Fusion                            (fusion)
import           LocalComputation.Utils                                       (fromRight)
import           LocalComputation.ValuationAlgebra.QuasiRegular               (solution)
import           Type.Reflection                                              (Typeable)

type Result a = M.LabelledMatrix a () TropicalSemiringValue
type Knowledgebase a = [Q.QuasiRegularValuation TropicalSemiringValue a ()]
type Query a = (a, a)

data InvalidGraph = MissingZeroCostSelfLoops deriving (NFData, Generic, Show)

-- If distance of a location to itself is not recorded, it will be recorded as the 'zero'
-- element of the tropical semiring (i.e. infinity). However, it still seems to determine
-- that each vertex has a 0 cost edge to itself.
knowledgeBase :: forall a . (H.Hashable a, Ord a) => [Graph a TropicalSemiringValue] -> a -> Knowledgebase a
knowledgeBase gs target = map f gs
    where
        f g = fromJust $ Q.create m b
            where
                m = M.toSquare (matrixFromGraph g) zero
                b = fromRight $ M.fromList [((a, ()), if a == target then one else zero) | a <- S.toList $ fst (M.domain m)]

        matrixFromGraph :: (Ord b, SemiringValue b) => Graph a b -> M.LabelledMatrix a a b
        matrixFromGraph g = fromRight $ M.fromListDefault zero (MP.toList $ rearrangedGraph g)

        -- Rearranges the graph from `MP.Map a [(a, b)]` to `MP.Map (a, a) b`.
        -- If multiple arcs exist between a node, retains only the minimum cost arc.
        rearrangedGraph :: Ord b => Graph a b -> MP.Map (a, a) b
        rearrangedGraph g = MP.fromListWith (\v1 v2 -> min v1 v2) (assocList g)

        -- Rearranges the graph into an associative list of ((arcHead, arcTail), cost)
        -- May contain duplicate entries if there may exist multiple arcs between a set of nodes.
        assocList :: Graph a b -> [((a, a), b)]
        assocList g = map (\e -> ((e.arcHead, e.arcTail), e.weight)) (G.toList g)

-- | Retuns a distance entry from the resulting valuation after inference. Unsafe.
getDistance :: (Ord a) => Result a -> Query a -> TropicalSemiringValue
getDistance x (source, _) = fromJust $ M.find (source, ()) x

-- TODO: Ensure caches result of 'solution'
-- TODO: Can this handle negative weights?
{- | Returns the shortest distance between a single target and multiple sources.

Assumes that every graph node can reach itself with 0 cost. This is a limitation of the inference process using quasi regular valuations;
consider the result of `Q.solution` on a `Q.LabelledMatrix (fromList [(0, 0), T 1]) (fromList [(0, ()), 0)`. Here, following the formula
for `Q.solution` and the quasi-inverse definition of a `TropicalSemiringValue` the edge cost `T 1` becomes `T 0`.

To make this assumption explicit, returns `Left InvalidGraph` if a graph that does not have 0 cost self loops is given.
-}
singleTarget :: (H.Hashable a, Binary a, Typeable a, Ord a, Show a) => [Graph a TropicalSemiringValue] -> [a] -> a -> Process (Either InvalidGraph [TropicalSemiringValue])
singleTarget = singleTarget'' ShenoyShafer

-- singleTarget' :: (H.Hashable a, Binary a, Typeable a, Ord a, Show a) => [Graph a TropicalSemiringValue] -> [a] -> a -> Process (Either InvalidGraph [TropicalSemiringValue])
-- singleTarget' = singleTarget'' Fusion

singleTarget' :: (H.Hashable a, Ord a, Show a) => [Graph a TropicalSemiringValue] -> a -> a -> Either InvalidGraph TropicalSemiringValue
singleTarget' vs source target
    | any (not . G.hasZeroCostSelfLoops) vs = Left MissingZeroCostSelfLoops
    | otherwise = Right $ getDistance (Q.solution result) (source, target)

    where
        k = knowledgeBase vs target
        domain = S.fromList [source, target]

        result = fromRight $ fusion k domain


data ComputationMode = Fusion | ShenoyShafer

singleTarget'' :: (Show a, Binary a, Typeable a, H.Hashable a, Ord a)
    => ComputationMode
    -> [Graph a TropicalSemiringValue]
    -> [a]
    -> a
    -> Process (Either InvalidGraph [TropicalSemiringValue])
singleTarget'' mode vs sources target
    | any (not . G.hasZeroCostSelfLoops) vs = pure $ Left MissingZeroCostSelfLoops
    | otherwise = do
        results <- fmap Q.solution $ case mode of
                                        ShenoyShafer -> answerQueryM k domain
                                        Fusion       -> pure $ fromRight $ fusion k domain
        pure $ Right $ map (\s -> getDistance results (s, target)) sources

    where
        k = knowledgeBase vs target
        domain = S.union (S.fromList sources) (S.singleton target)


-- | Old single target algorithm that utilizes shenoy shafer and multiple single-target queries to return the result.
oldSingleTarget :: (H.Hashable a, Binary a, Typeable a, Ord a, Show a) => [Graph a TropicalSemiringValue] -> [a] -> a -> Process (Either InvalidGraph [TropicalSemiringValue])
oldSingleTarget vs sources target
    | any (not . G.hasZeroCostSelfLoops) vs = pure $ Left MissingZeroCostSelfLoops
    | otherwise = do
        results <- answerQueriesM k domains
        pure $ Right $ map (\(s, r) -> getDistance r (s, target)) $ zip sources (map Q.solution results)

        where
            k = knowledgeBase vs target
            domains = map (\s -> S.fromList [s, target]) sources
