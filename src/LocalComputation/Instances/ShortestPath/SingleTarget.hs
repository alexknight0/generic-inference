{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LocalComputation.Instances.ShortestPath.SingleTarget
    (
      singleTarget
    , singleTargetTmp
    , Error (MissingZeroCostSelfLoops)
    , Query (..)
    )
where

import           Control.Distributed.Process                                  (Process)
import           Data.Maybe                                                   (fromJust)
import qualified Data.Set                                                     as S

import qualified Data.Map                                                     as MP
import           LocalComputation.Inference.ShenoyShafer                      (answerQueriesM)
import qualified LocalComputation.LabelledMatrix                              as M
import qualified LocalComputation.ValuationAlgebra.QuasiRegular               as Q (QuasiRegularValuation,
                                                                                    SemiringValue (one, zero),
                                                                                    TropicalSemiringValue,
                                                                                    create,
                                                                                    solution)
-- Typeclasses
import           Control.DeepSeq                                              (NFData)
import           Control.Monad.IO.Class                                       (MonadIO)
import           Data.Binary                                                  (Binary)
import qualified Data.Hashable                                                as H
import           GHC.Generics                                                 (Generic)
import           LocalComputation.Graph                                       as G
import qualified LocalComputation.Inference                                   as I
import qualified LocalComputation.Inference.Fusion                            as F
import           LocalComputation.Utils                                       (fromRight)
import qualified LocalComputation.ValuationAlgebra.QuasiRegular.SemiringValue as Q (TropicalSemiringValue (..),
                                                                                    toDouble)
import           Type.Reflection                                              (Typeable)

type Result a = M.LabelledMatrix a () Q.TropicalSemiringValue
type Knowledgebase a = [Q.QuasiRegularValuation Q.TropicalSemiringValue a]

-- | Query for a multiple-source single-target problem.
data Query a = Query { sources :: [a], target :: a } deriving Show

data Error = InferenceError I.Error | MissingZeroCostSelfLoops deriving (NFData, Generic, Show)

-- If distance of a location to itself is not recorded, it will be recorded as the 'zero'
-- element of the tropical semiring (i.e. infinity). Regarding self loops, see the documentation
-- of `singleTarget`.
knowledgeBase :: forall a . (H.Hashable a, Ord a) => [Graph a Q.TropicalSemiringValue] -> a -> Knowledgebase a
knowledgeBase gs target = map f gs
    where
        f g = fromJust $ Q.create m b
            where
                m = M.toSquare (matrixFromGraph g) Q.zero
                b = fromRight $ M.fromList [((a, ()), if a == target then Q.one else Q.zero) | a <- S.toList $ fst (M.domain m)]

        matrixFromGraph :: (Ord b, Q.SemiringValue b) => Graph a b -> M.LabelledMatrix a a b
        matrixFromGraph g = fromRight $ M.fromListDefault Q.zero (MP.toList $ rearrangedGraph g)

        -- Rearranges the graph from `MP.Map a [(a, b)]` to `MP.Map (a, a) b`.
        -- If multiple arcs exist between a node, retains only the minimum cost arc.
        rearrangedGraph :: Ord b => Graph a b -> MP.Map (a, a) b
        rearrangedGraph g = MP.fromListWith (\v1 v2 -> min v1 v2) (assocList g)

        -- Rearranges the graph into an associative list of ((arcHead, arcTail), cost)
        -- May contain duplicate entries if there may exist multiple arcs between a set of nodes.
        assocList :: Graph a b -> [((a, a), b)]
        assocList g = map (\e -> ((e.arcHead, e.arcTail), e.weight)) (G.toList g)

-- | Retuns a distance entry from the resulting valuation after inference. Unsafe.
getDistance :: (Ord a) => Result a -> (a, a) -> Q.TropicalSemiringValue
getDistance x (source, _) = fromJust $ M.find (source, ()) x

{- | Returns the shortest distance between a single target and multiple sources.

Assumes that every graph node can reach itself with 0 cost. This is a limitation of the inference process using quasi regular valuations;
consider the result of `Q.solution` on a `Q.LabelledMatrix (fromList [(0, 0), T 1]) (fromList [(0, ()), 0)`. Here, following the formula
for `Q.solution` and the quasi-inverse definition of a `Q.TropicalSemiringValue` the edge cost `T 1` becomes `T 0`.

To make this assumption explicit, returns `Left InvalidGraph` if a graph that does not have 0 cost self loops is given.
-}
singleTarget :: (NFData a, MonadIO m, Show a, Binary a, Typeable a, H.Hashable a, Ord a)
    => I.Mode
    -> [Graph a Double]
    -> [a]
    -> a
    -> Either Error (m [Double])
singleTarget mode vs sources target = fmap (fmap (map Q.toDouble)) $ singleTarget' mode (map (fmap Q.T) vs) sources target

-- | Returns the answers to multiple single-target queries.
--
-- Computes the solution by performing multiple seperate single target computations, even in the shenoy case;
-- using shenoy here allows computing multiple sources in one inference sweep, but not computing multiple targets.
singleTargets :: forall a m . (NFData a, MonadIO m, Show a, Binary a, Typeable a, H.Hashable a, Ord a)
    => I.Mode
    -> [Graph a Double]
    -> [Query a]
    -> Either Error (m [[Double]])
singleTargets mode gs qs = fmap sequence $ mapM (\q -> singleTarget mode gs q.sources q.target) qs

-- TODO: Can this handle negative weights?
singleTarget' :: (NFData a, MonadIO m, Show a, Binary a, Typeable a, H.Hashable a, Ord a)
    => I.Mode
    -> [Graph a Q.TropicalSemiringValue]
    -> [a]
    -> a
    -> Either Error (m [Q.TropicalSemiringValue])
singleTarget' mode vs sources target
    | any (not . G.hasZeroCostSelfLoops) vs = Left  $ MissingZeroCostSelfLoops
    | Left e          <- solutionMM         = Left  $ InferenceError e
    | Right solutionM <- solutionMM         = Right $ do
        solution <- solutionM
        pure $ map (\s -> getDistance solution (s, target)) sources

    where
        k = knowledgeBase vs target
        domain = S.fromList (target : sources)

        solutionMM = fmap (fmap Q.solution) $ I.query mode k domain

singleTargetTmp :: (NFData a, MonadIO m, Show a, Binary a, Typeable a, H.Hashable a, Ord a)
    => FilePath
    -> [Graph a Double]
    -> [a]
    -> a
    -> Either Error (m [Double])
singleTargetTmp filepath vs sources target = fmap (fmap (map Q.toDouble)) $ singleTargetTmp' filepath (map (fmap Q.T) vs) sources target


singleTargetTmp' :: (NFData a, MonadIO m, Show a, Binary a, Typeable a, H.Hashable a, Ord a)
    => FilePath
    -> [Graph a Q.TropicalSemiringValue]
    -> [a]
    -> a
    -> Either Error (m [Q.TropicalSemiringValue])
singleTargetTmp' filepath vs sources target
    | any (not . G.hasZeroCostSelfLoops) vs = Left  $ MissingZeroCostSelfLoops
    | Left e          <- solutionMM         = Left  $ InferenceError e
    | Right solutionM <- solutionMM         = Right $ do
        solution <- solutionM
        pure $ map (\s -> getDistance solution (s, target)) sources

    where
        k = knowledgeBase vs target
        domain = S.fromList (target : sources)

        solutionMM = fmap (fmap Q.solution) $ I.queryDrawGraph filepath I.Shenoy k domain

-- TODO: How do we get 'inferred results' from fusion?
--
-- singleTarget'' :: (NFData a, MonadIO m, Show a, Binary a, Typeable a, H.Hashable a, Ord a)
--     => [Graph a Q.TropicalSemiringValue]
--     -> [a]
--     -> a
--     -> Either Error (m [Q.TropicalSemiringValue])
-- singleTarget'' vs sources target
--     | any (not . G.hasZeroCostSelfLoops) vs = Left  $ MissingZeroCostSelfLoops
--     | otherwise                             = Right $ do
--         solution <- solutionM
--         pure $ map (\s -> getDistance solution (s, target)) sources
--
--     where
--         k = knowledgeBase vs target
--         domains = map (\s -> S.fromList [s, target]) sources
--
--         solutionMM = undefined
--         test = mapM (\q -> F.fusion vs q) domains


-- | Old single target algorithm that utilizes shenoy shafer and multiple single-target queries to return the result.
oldSingleTarget :: (H.Hashable a, Binary a, Typeable a, Ord a, Show a) => [Graph a Q.TropicalSemiringValue] -> [a] -> a -> Process (Either Error [Q.TropicalSemiringValue])
oldSingleTarget vs sources target
    | any (not . G.hasZeroCostSelfLoops) vs = pure $ Left MissingZeroCostSelfLoops
    | otherwise = do
        results <- answerQueriesM k domains
        pure $ Right $ map (\(s, r) -> getDistance r (s, target)) $ zip sources (map Q.solution results)

        where
            k = knowledgeBase vs target
            domains = map (\s -> S.fromList [s, target]) sources
