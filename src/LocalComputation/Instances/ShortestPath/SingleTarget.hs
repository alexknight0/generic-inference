{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LocalComputation.Instances.ShortestPath.SingleTarget
    (
      singleTarget
    , singleTargetDP
    , singleTargets
    , Query (..)
    )
where

import           Data.Maybe                                           (fromJust)
import qualified Data.Set                                             as S

import qualified Data.Map                                             as MP
import qualified LocalComputation.LabelledMatrix                      as M
import qualified LocalComputation.ValuationAlgebra.QuasiRegular       as Q (SemiringValue,
                                                                            TropicalSemiringValue,
                                                                            Valuation,
                                                                            create,
                                                                            one,
                                                                            solution,
                                                                            zero)
-- Typeclasses
import           Control.DeepSeq                                      (NFData)
import           Control.Monad.IO.Class                               (MonadIO)
import           Data.Binary                                          (Binary)
import qualified Data.Hashable                                        as H
import           LocalComputation.Graph                               as G
import qualified LocalComputation.Inference                           as I
import qualified LocalComputation.Inference.JoinTree.Diagram          as D
import           LocalComputation.Utils                               (fromRight)
import qualified LocalComputation.ValuationAlgebra                    as V
import qualified LocalComputation.ValuationAlgebra.QuasiRegular.Value as Q (TropicalSemiringValue (..),
                                                                            toDouble)
import           Type.Reflection                                      (Typeable)

-- TODO: A notable property here seems to be that we don't need to find the shortest path from 'all' nodes;
-- by restricting the domain we can find the shortest path from a few targets - something you can't do with
-- djikstra?

-- TODO: Where does the hashable constraint come from?

type Result a = M.LabelledMatrix a () Q.TropicalSemiringValue
type Knowledgebase a = [Q.Valuation Q.TropicalSemiringValue a]

-- | Query for a multiple-source single-target problem.
data Query a = Query { sources :: [a], target :: a } deriving Show

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

type ComputeInference m a = D.DrawSettings -> [Q.Valuation Q.TropicalSemiringValue a] -> V.Domain a -> Either I.Error (m (M.LabelledMatrix a () Q.TropicalSemiringValue))

-- TODO: Can this handle negative weights?

{- | Returns the shortest distance between a single target and multiple sources.

__Warning__ : The shortest path from a vertex to itself is 0 (the trivial path).
-}
singleTarget :: (NFData a, MonadIO m, Show a, Binary a, Typeable a, H.Hashable a, Ord a)
    => D.DrawSettings
    -> I.Mode
    -> [Graph a Double]
    -> Query a
    -> Either I.Error (m [Double])
singleTarget settings mode gs q = usingDouble (singleTarget' inference) settings gs q
    where
        inference s k domain = fmap (fmap Q.solution) $ I.queryDrawGraph s mode k domain

singleTarget' :: ( MonadIO m, Show a, H.Hashable a, Ord a)
    => ComputeInference m a
    -> D.DrawSettings
    -> [Graph a Q.TropicalSemiringValue]
    -> Query a
    -> Either I.Error (m [Q.TropicalSemiringValue])
singleTarget' inference settings vs q = do
    solutionM <- solutionMOrError

    pure $ do
        solution <- solutionM
        pure $ map (\s -> getDistance solution (s, q.target)) q.sources

    where
        k = knowledgeBase vs q.target
        domain = S.fromList (q.target : q.sources)

        solutionMOrError = inference settings k domain

singleTargetDP :: (NFData a, MonadIO m, Show a, Binary a, Typeable a, H.Hashable a, Ord a)
    => D.DrawSettings
    -> [Graph a Double]
    -> Query a
    -> Either I.Error (m [Double])
singleTargetDP = usingDouble (singleTarget' I.queryDPDrawGraph)

--------------------------------------------------------------------------------
-- Multiple query variants
--------------------------------------------------------------------------------

-- | Returns the answers to multiple single-target queries.
--
-- Computes the solution by performing multiple seperate single target computations, even in the shenoy case;
-- using shenoy here allows computing multiple sources in one inference sweep, but not computing multiple targets.
singleTargets :: forall a m . (NFData a, MonadIO m, Show a, Binary a, Typeable a, H.Hashable a, Ord a)
    => D.DrawSettings
    -> I.Mode
    -> [Graph a Double]
    -> [Query a]
    -> Either I.Error (m [[Double]])
singleTargets s mode gs qs = fmap sequence $ mapM (\q -> singleTarget s mode gs q) qs

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Converts a function that operates using tropical semiring values to operate using doubles
usingDouble :: (Functor m)
    => (D.DrawSettings -> [Graph a Q.TropicalSemiringValue] -> Query a -> Either I.Error (m [Q.TropicalSemiringValue]))
    -> (D.DrawSettings -> [Graph a Double]                  -> Query a -> Either I.Error (m [Double]))
usingDouble f s vs qs = fmap (fmap (map Q.toDouble)) $ f s (map (fmap Q.T) vs) qs

