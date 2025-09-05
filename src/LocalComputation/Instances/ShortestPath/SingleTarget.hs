{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LocalComputation.Instances.ShortestPath.SingleTarget
    (
      singleTarget
    , singleTargetTmp
    , Error (MissingZeroCostSelfLoops)
    , Query (..)
    , singleTargetConfigSet
    )
where

import           Control.Distributed.Process                                  (Process)
import           Data.Maybe                                                   (fromJust)
import qualified Data.Set                                                     as S

import qualified Data.Map                                                     as MP
import qualified LocalComputation.LabelledMatrix                              as M
import qualified LocalComputation.ValuationAlgebra.QuasiRegular               as Q (QuasiRegularValuation,
                                                                                    SemiringValue (one, zero),
                                                                                    TropicalSemiringValue,
                                                                                    create,
                                                                                    singleSolutionCompute,
                                                                                    solution)
-- Typeclasses
import           Control.DeepSeq                                              (NFData)
import           Control.Monad.IO.Class                                       (MonadIO,
                                                                               liftIO)
import           Data.Binary                                                  (Binary)
import qualified Data.Hashable                                                as H
import           GHC.Generics                                                 (Generic)
import           LocalComputation.Graph                                       as G
import qualified LocalComputation.Inference                                   as I
import qualified LocalComputation.Inference.Fusion                            as F
import qualified LocalComputation.Inference.JoinTree.Diagram                  as D
import           LocalComputation.LocalProcess                                (run)
import           LocalComputation.Utils                                       (fromRight)
import qualified LocalComputation.ValuationAlgebra                            as V
import qualified LocalComputation.ValuationAlgebra.QuasiRegular.SemiringValue as Q (TropicalSemiringValue (..),
                                                                                    toDouble)
import           Type.Reflection                                              (Typeable)

-- TODO: A notable property here seems to be that we don't need to find the shortest path from 'all' nodes;
-- by restricting the domain we can find the shortest path from a few targets.

type Result a = M.LabelledMatrix a () Q.TropicalSemiringValue
type Knowledgebase a = [Q.QuasiRegularValuation Q.TropicalSemiringValue a]

-- | Query for a multiple-source single-target problem.
data Query a = Query { sources :: [a], target :: a } deriving Show

data Error = InferenceError I.Error | MissingZeroCostSelfLoops deriving (NFData, Generic, Show)

-- If distance of a location to itself is not recorded, it will be recorded as the 'zero'
-- element of the tropical semiring (i.e. infinity). Regarding self loops, see the documentation
-- of `singleTarget`.
knowledgeBase :: forall a . (H.Hashable a, V.Var a) => [Graph a Q.TropicalSemiringValue] -> a -> Knowledgebase a
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

singleTargetConfigSet :: (NFData a, MonadIO m, Show a, Binary a, Typeable a, H.Hashable a, Ord a)
    => D.DrawSettings
    -> I.Mode
    -> [Graph a Double]
    -> [a]
    -> a
    -> Either Error (m [Double])
singleTargetConfigSet s mode vs sources target = fmap (fmap (map Q.toDouble)) $ singleTargetConfigSet' s mode (map (fmap Q.T) vs) sources target


singleTargetConfigSet' :: (NFData a, MonadIO m, Show a, Binary a, Typeable a, H.Hashable a, Ord a)
    => D.DrawSettings
    -> I.Mode
    -> [Graph a Q.TropicalSemiringValue]
    -> [a]
    -> a
    -> Either Error (m [Q.TropicalSemiringValue])
singleTargetConfigSet' s mode vs sources target
    | any (not . G.hasZeroCostSelfLoops) vs = Left  $ MissingZeroCostSelfLoops
    | otherwise = Right $ do
        solution <- liftIO $ run solutionM
        pure $ map (\s -> getDistance solution (s, target)) sources

    where
        k = knowledgeBase vs target
        domain = S.fromList (target : sources)

        solutionM = fmap Q.singleSolutionCompute $ F.fusionPass s k domain

-- TODO: Used for drawing graphs. Remove once our interface is better.
singleTargetTmp :: (NFData a, MonadIO m, Show a, Binary a, Typeable a, H.Hashable a, Ord a)
    => D.DrawSettings
    -> [Graph a Double]
    -> [a]
    -> a
    -> Either Error (m [Double])
singleTargetTmp s vs sources target = fmap (fmap (map Q.toDouble)) $ singleTargetTmp' s (map (fmap Q.T) vs) sources target


-- TODO: Used for drawing graphs. Remove once our interface is better.
singleTargetTmp' :: (NFData a, MonadIO m, Show a, Binary a, Typeable a, H.Hashable a, Ord a)
    => D.DrawSettings
    -> [Graph a Q.TropicalSemiringValue]
    -> [a]
    -> a
    -> Either Error (m [Q.TropicalSemiringValue])
singleTargetTmp' settings vs sources target
    | any (not . G.hasZeroCostSelfLoops) vs = Left  $ MissingZeroCostSelfLoops
    | Left e          <- solutionMM         = Left  $ InferenceError e
    | Right solutionM <- solutionMM         = Right $ do
        solution <- solutionM
        pure $ map (\s -> getDistance solution (s, target)) sources

    where
        k = knowledgeBase vs target
        domain = S.fromList (target : sources)

        solutionMM = fmap (fmap Q.solution) $ I.queryDrawGraph settings I.Shenoy k domain

