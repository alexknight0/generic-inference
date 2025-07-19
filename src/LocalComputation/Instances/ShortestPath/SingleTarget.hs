{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module LocalComputation.Instances.ShortestPath.SingleTarget
    (
      singleTarget
    , InvalidGraph (MissingZeroCostSelfLoops)
    )
where

import           Control.Distributed.Process                                  (Process)
import           Data.Maybe                                                   (fromJust)
import qualified Data.Set                                                     as S

import qualified Data.Map                                                     as MP
import           LocalComputation.Inference.ShenoyShafer                      (answerQueriesM)
import qualified LocalComputation.LabelledMatrix                              as M
import qualified LocalComputation.ValuationAlgebra.QuasiRegular               as Q (QuasiRegularValuation,
                                                                                    create,
                                                                                    solution)
import           LocalComputation.ValuationAlgebra.QuasiRegular.SemiringValue

-- Typeclasses
import           Control.DeepSeq                                              (NFData)
import           Data.Binary                                                  (Binary)
import           GHC.Generics                                                 (Generic)
import           LocalComputation.Graph                                       as G
import           LocalComputation.Utils                                       (fromRight)
import           Type.Reflection                                              (Typeable)

type Knowledgebase a = [Q.QuasiRegularValuation TropicalSemiringValue a ()]
type Query a = (a, a)

data InvalidGraph = MissingZeroCostSelfLoops deriving (NFData, Generic)

-- If distance of a location to itself is not recorded, it will be recorded as the 'zero'
-- element of the tropical semiring (i.e. infinity). However, it still seems to determine
-- that each vertex has a 0 cost edge to itself.
knowledgeBase :: forall a . (Ord a) => [Graph a TropicalSemiringValue] -> a -> Knowledgebase a
knowledgeBase gs target = map f gs
    where
        f g = fromJust $ Q.create m b
            where
                m = M.toSquare (matrixFromGraph g) zero
                b = fromRight $ M.fromList [((a, ()), if a == target then one else zero) | a <- S.toList $ fst (M.domain m)]

        matrixFromGraph :: (Ord b, SemiringValue b) => Graph a b -> M.LabelledMatrix a a b
        matrixFromGraph g = fromRight $ M.fromListDefault (MP.toList $ rearrangedGraph g) zero

        -- Rearranges the graph from `MP.Map a [(a, b)]` to `MP.Map (a, a) b`.
        -- If multiple arcs exist between a node, retains only the minimum cost arc.
        rearrangedGraph :: Ord b => Graph a b -> MP.Map (a, a) b
        rearrangedGraph g = MP.fromListWith (\v1 v2 -> min v1 v2) (assocList g)

        -- Rearranges the graph into an associative list of ((arcHead, arcTail), cost)
        -- May contain duplicate entries if there may exist multiple arcs between a set of nodes.
        assocList :: Graph a b -> [((a, a), b)]
        assocList g = map (\e -> ((e.arcHead, e.arcTail), e.weight)) (G.toList g)

-- | Retuns a distance entry from the resulting valuation after inference. Unsafe.
getDistance :: (Show a, Ord a) => Q.QuasiRegularValuation TropicalSemiringValue a () -> Query a -> TropicalSemiringValue
getDistance x (source, _) = fromJust $ M.find (source, ()) (Q.solution x)

-- TODO: Can this handle negative weights?
{- | Returns the shortest distance between a single target and multiple sources.

Assumes that every graph node can reach itself with 0 cost. This is a limitation of the inference process using quasi regular valuations;
consider the result of `Q.solution` on a `Q.LabelledMatrix (fromList [(0, 0), T 1]) (fromList [(0, ()), 0)`. Here, following the formula
for `Q.solution` and the quasi-inverse definition of a `TropicalSemiringValue` the edge cost `T 1` becomes `T 0`.

To make this assumption explicit, returns `Left InvalidGraph` if a graph that does not have 0 cost self loops is given.
-}
singleTarget :: (Binary a, Typeable a, Ord a, Show a) => [Graph a TropicalSemiringValue] -> [a] -> a -> Process (Either InvalidGraph [TropicalSemiringValue])
singleTarget vs sources target
    | any (not . G.hasZeroCostSelfLoops) vs = pure $ Left MissingZeroCostSelfLoops
    | otherwise = do
        results <- answerQueriesM k domains
        pure $ Right $ map (\(s, r) -> getDistance r (s, target)) $ zip sources results

        where
            k = knowledgeBase vs target
            domains = map (\s -> S.fromList [s, target]) sources

