{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LocalComputation.Instances.ShortestPath.SingleTarget (
      singleTarget
    , singleTargetDP
    , singleTargetSplit
    , singleTargetSplitDP
    , singleTargetsSplit
    , Query (..)
    , decomposition
) where

import           Data.Maybe                                           (fromJust)
import qualified Data.Set                                             as S

import qualified Data.Map                                             as MP
import qualified LocalComputation.LabelledMatrix                      as M
import qualified LocalComputation.ValuationAlgebra.QuasiRegular       as Q (SemiringValue,
                                                                            TropicalSemiringValue,
                                                                            Valuation,
                                                                            one,
                                                                            solution,
                                                                            unsafeCreate,
                                                                            zero)
-- Typeclasses
import           Control.DeepSeq                                      (NFData)
import           Control.Monad.IO.Class                               (MonadIO)
import qualified Data.Bifunctor                                       as B
import           Data.Binary                                          (Binary)
import qualified Data.Hashable                                        as H
import qualified Data.List                                            as L
import qualified Data.Map                                             as Map
import qualified LocalComputation.Graph                               as G
import qualified LocalComputation.Graph.Undirected                    as UG
import qualified LocalComputation.Inference                           as I
import qualified LocalComputation.Inference.EliminationSequence       as E
import qualified LocalComputation.Inference.JoinTree.Diagram          as D
import qualified LocalComputation.Inference.MessagePassing            as MP
import qualified LocalComputation.Inference.Statistics                as S
import qualified LocalComputation.Inference.Triangulation             as T
import           LocalComputation.Utils                               (fromRight)
import qualified LocalComputation.Utils                               as U
import qualified LocalComputation.ValuationAlgebra                    as V
import qualified LocalComputation.ValuationAlgebra.QuasiRegular.Value as Q (TropicalSemiringValue (..),
                                                                            toDouble)
import           Type.Reflection                                      (Typeable)

-- TODO: Where does the hashable constraint come from?

-- | Query for a multiple-source single-target problem.
data Query a = Query { sources :: [a], target :: a } deriving (Show, V.Generic, NFData)

{- | Returns the shortest distance between a single target and multiple sources.

__Warning__ : The shortest path from a vertex to itself is 0 (the trivial path).
-}
singleTargetSplit :: (NFData a, MonadIO m, Show a, Binary a, Typeable a, H.Hashable a, Ord a)
    => I.Mode
    -> D.DrawSettings
    -> [G.Graph a Double]
    -> Query a
    -> Either I.Error (m (S.WithStats [Double]))
singleTargetSplit mode settings gs q = usingDouble (singleTargetSplitGeneric inference) settings gs q
    where
        -- We get inference results by doing a query then calling `solution` on the result.
        inference s k domain = fmap (fmap (fmap Q.solution)) $ I.queryWithStats s mode k domain

singleTargetSplitDP :: (NFData a, MonadIO m, Show a, Binary a, Typeable a, H.Hashable a, Ord a)
    => MP.Mode
    -> D.DrawSettings
    -> [G.Graph a Double]
    -> Query a
    -> Either I.Error (m (S.WithStats [Double]))
singleTargetSplitDP mode = usingDouble $ singleTargetSplitGeneric $ (\s -> I.solutionWithStats s mode)

singleTargetSplitGeneric :: ( MonadIO m, Show a, H.Hashable a, Ord a)
    => ComputeInference m a
    -> D.DrawSettings
    -> [G.Graph a Q.TropicalSemiringValue]
    -> Query a
    -> Either I.Error (m (S.WithStats [Q.TropicalSemiringValue]))
singleTargetSplitGeneric inference settings vs q = do
    solutionM <- solutionMOrError

    pure $ do
        solution <- solutionM
        pure $ S.withStats solution.stats $ map (\s -> unsafeGetDistance solution.c (s, q.target)) q.sources

    where
        k = knowledgeBase vs q.target
        domain = S.fromList (q.target : q.sources)

        solutionMOrError = inference settings k domain

-- If distance of a location to itself is not recorded, it will be recorded as the 'zero'
-- element of the tropical semiring (i.e. infinity). Regarding self loops, see the documentation
-- of `singleTarget`.
knowledgeBase :: forall a . (H.Hashable a, V.Var a) => [G.Graph a Q.TropicalSemiringValue] -> a -> [Q.Valuation Q.TropicalSemiringValue a]
knowledgeBase gs target = map f gs
    where
        f g = Q.unsafeCreate m b
            where
                m = M.toSquare (matrixFromGraph g) Q.zero
                b = fromRight $ M.fromList [((a, ()), if a == target then Q.one else Q.zero) | a <- S.toList $ fst (M.domain m)]

        matrixFromGraph :: (Ord b, Q.SemiringValue b) => G.Graph a b -> M.LabelledMatrix a a b
        matrixFromGraph g = M.fromListDefault Q.zero (MP.toList $ rearrangedGraph g)

        -- Rearranges the graph from `MP.Map a [(a, b)]` to `MP.Map (a, a) b`.
        -- If multiple arcs exist between a node, retains only the minimum cost arc.
        rearrangedGraph :: Ord b => G.Graph a b -> MP.Map (a, a) b
        rearrangedGraph g = MP.fromListWith (\v1 v2 -> min v1 v2) (assocList g)

        -- Rearranges the graph into an associative list of ((arcHead, arcTail), cost)
        -- May contain duplicate entries if there may exist multiple arcs between a set of nodes.
        assocList :: G.Graph a b -> [((a, a), b)]
        assocList g = map (\e -> ((e.arcHead, e.arcTail), e.weight)) (G.toEdgeList g)

-- | A function that given some draw settings, a knowledgebase and a query, computes and returns the inference results.
type ComputeInference m a = D.DrawSettings
                         -> [Q.Valuation Q.TropicalSemiringValue a]
                         -> V.Domain a
                         -> Either I.Error (m (S.WithStats (M.LabelledMatrix a () Q.TropicalSemiringValue)))

--------------------------------------------------------------------------------
-- Decomposition
--------------------------------------------------------------------------------
-- TODO: Decomposition technically will duplicate edges.
-- i.e. if we have cliques {A, B, C} and cliques {B, C, D} then edges between 'b' and 'c' will be present
-- in multiple graphs, even though we could communicate that information by only including the
-- edges in one graph. Would solving this problem may result in speedups?
decomposition :: forall a b . (Show a, Ord a, Eq b) => G.Graph a b -> [G.Graph a b]
decomposition g = oldDecomposition g -- map (\c -> G.induce (`S.member` c) g) cliques
    where
        cliques = U.removeSubsets $ T.maximalCliques (UG.fromGraph g)

-- TODO: Add to future work: a more sophisticated decomposition algorithm
oldDecomposition :: forall a b . (Ord a) => G.Graph a b -> [G.Graph a b]
oldDecomposition g = filter (not . G.isEmpty) $
                    decomposition' (E.create $ map (.neighbourhood) nHoods) nHoods
    where
        decomposition' :: E.EliminationSequence a -> [Vertex a] -> [G.Graph a b]
        decomposition' _ []      = []
        decomposition' e vertices = graph : decomposition' e' remaining
            where
                (vertex, e') = fromJust (E.eliminateNext e)

                (containingVertex, remaining) = L.partition (\v -> vertex `elem` v.neighbourhood) vertices

                graph = G.outgoingSubgraph g (S.fromList $ map (.v) containingVertex)

        nHoods = neighbourhoods g

data Vertex a = Vertex { v :: a, neighbourhood :: S.Set a }

-- | Returns some 'cliques' behind a graph for use with decomposition.
-- Really just a list of sets containing the neighbours of each node (plus the node itself).
neighbourhoods :: (Ord a) => G.Graph a b -> [Vertex a]
neighbourhoods g = map (\(n, adjacents) -> Vertex n (S.insert n adjacents)) neighbours
    where
        neighbours = Map.toList $ G.neighbourMap g

--------------------------------------------------------------------------------
-- Unsplit variants
--------------------------------------------------------------------------------
{- | Returns the shortest distance between a single target and multiple sources.

__Warning__ : The shortest path from a vertex to itself is 0 (the trivial path).
-}
singleTarget :: (NFData a, MonadIO m, Show a, Binary a, Typeable a, H.Hashable a, Ord a)
    => I.Mode
    -> D.DrawSettings
    -> G.Graph a Double
    -> Query a
    -> Either I.Error (m (S.WithStats [Double]))
singleTarget mode settings g = singleTargetSplit mode settings (decomposition g)

singleTargetDP :: (NFData a, MonadIO m, Show a, Binary a, Typeable a, H.Hashable a, Ord a)
    => MP.Mode
    -> D.DrawSettings
    -> G.Graph a Double
    -> Query a
    -> Either I.Error (m (S.WithStats [Double]))
singleTargetDP mode settings g = singleTargetSplitDP mode settings (decomposition g)

--------------------------------------------------------------------------------
-- Multiple query variants
--------------------------------------------------------------------------------

-- | Returns the answers to multiple single-target queries.
--
-- Computes the solution by performing multiple seperate single target computations, even in the shenoy case;
-- using shenoy here allows computing multiple sources in one inference sweep, but not computing multiple targets.
singleTargetsSplit :: forall a m . (NFData a, MonadIO m, Show a, Binary a, Typeable a, H.Hashable a, Ord a)
    => I.Mode
    -> D.DrawSettings
    -> [G.Graph a Double]
    -> [Query a]
    -> Either I.Error (m (S.WithStats [[Double]]))
singleTargetsSplit mode s gs qs = fmap (fmap S.lift . sequence) $ mapM (\q -> singleTargetSplit mode s gs q) qs

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------
-- | Retuns a distance entry from the resulting valuation after inference. Unsafe.
unsafeGetDistance :: (Ord a) => M.LabelledMatrix a () Q.TropicalSemiringValue -> (a, a) -> Q.TropicalSemiringValue
unsafeGetDistance x (source, _) = fromJust $ M.find (source, ()) x

-- | Converts a function that operates using tropical semiring values to operate using doubles
usingDouble :: (Ord a, Functor m)
    => (D.DrawSettings -> [G.Graph a Q.TropicalSemiringValue] -> Query a -> Either I.Error (m (S.WithStats [Q.TropicalSemiringValue])))
    -> (D.DrawSettings -> [G.Graph a Double]                  -> Query a -> Either I.Error (m (S.WithStats [Double])))
usingDouble f s vs qs = fmap (fmap (fmap (map Q.toDouble))) $ f s (map (fmap Q.T) vs) qs

