module Benchmarks.ShortestPath.SingleTarget (
      benchmarks
    , singleTarget
    , singleTargets
    , singleTarget'
    , singleTargets'
    , singleTargetSplit
    , singleTargetsSplit
    , singleTargetSplit'
    , singleTargetsSplit'
    , Implementation (..)
    , allImplementations
    , allButBaseline
) where

-- TODO: !!!!!!!!!!!!!!!! BEFORE BENCHMARKING !!!!!!!!!!!!!!!!!!!!!
-- TODO: !!!!!!!!!!!!!!!! BEFORE BENCHMARKING !!!!!!!!!!!!!!!!!!!!!
-- TODO: !!!!!!!!!!!!!!!! BEFORE BENCHMARKING !!!!!!!!!!!!!!!!!!!!!
-- 1. Make fusion construct a better join tree for itself.
-- And after:
-- 1. Test singleTarget with a multiple query architecture, splitting
--    the 'domain' into pairs of (target, source) instead of a single
--    large query.
-- 2. Test with different numbers of calls
-- 3. Investigate garbage collection pressure

import qualified Benchmarks.ShortestPath.SingleTarget.Data            as D

import qualified Benchmarks.ShortestPath.SingleTarget.Baseline        as H
import           Criterion.Main
import qualified LocalComputation.Graph                               as G
import qualified LocalComputation.Inference                           as I
import qualified LocalComputation.Instances.ShortestPath.SingleTarget as ST
import           LocalComputation.Utils                               (fromRight,
                                                                       infinity)

import qualified Benchmarks.Utils                                     as U
import           Control.DeepSeq                                      (deepseq)
import qualified Control.Monad                                        as M
import           Control.Monad.IO.Class                               (MonadIO)
import qualified Data.Hashable                                        as H
import qualified LocalComputation.Inference.JoinTree.Diagram          as D
import qualified LocalComputation.Inference.MessagePassing            as MP
import qualified LocalComputation.ValuationAlgebra                    as V

--------------------------------------------------------------------------------
-- Benchmarks
--------------------------------------------------------------------------------

benchmarks :: IO [Benchmark]
benchmarks = do
    problems <- sequence $ zipWith U.sample seeds [ D.genProblem 5  10  10
                                                  , D.genProblem 30 100 10
                                                  , D.genProblem 50 100 10
                                               ]

    let algorithm mode = multipleSingleTargets mode D.def problems

    bruteForce  <- algorithm (Generic I.BruteForce)
    fusion      <- algorithm (Generic I.Fusion)
    shenoy      <- algorithm (Generic (I.Shenoy MP.Threads))
    -- baseline    <- algorithm (Baseline)

    pure $ pure $ bgroup "Shortest Path" [
                -- bench "localcomputation-bruteForce" $ nfIO bruteForce
                -- , bench "localcomputation-fusion"   $ nfIO fusion
                bench "localcomputation-shenoy"     $ nfIO shenoy
                -- , bench "djikstra"                    $ nfIO baseline
            ]
    where
        seeds = [0..]


--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------
-- | Choice between either a `Baseline` inference implementation taken from hackage,
-- a `Generic` inference implementation from this library, or the yet-to-be-generalized
-- dynamic programming implementation `DynamicP` dynamic programming implementation
-- from this library.
data Implementation = Baseline | Generic { _mode :: I.Mode } | DynamicP { _mode' :: MP.Mode } deriving (Eq, Show)

allImplementations :: [Implementation]
allImplementations = [ Baseline
                     , Generic  $ I.BruteForce
                     , Generic  $ I.Fusion
                     , Generic  $ I.Shenoy MP.Threads
                     , Generic  $ I.Shenoy MP.Distributed
                     , DynamicP $ MP.Distributed
                    ]

allButBaseline :: [Implementation]
allButBaseline = filter (/= Baseline) allImplementations

-- | A uniform interface for querying the single target shortest path solution using any implementation.
-- Returns an IO action that returns another IO action. When the initial IO action is bound it
-- performs the setup for problem and then returns an IO action that performs the computation that
-- actually needs to be benchmarked.
--
-- Takes multiple queries. No single target implementation currently supports solving multiple queries
-- faster than solving a single one multiple times; but by taking multiple queries we can benchmark
-- a function on the same graph without recreating the graph.
--
-- Technically unsafe - if one of the given queries is not answerable for the problem, will
-- (i.e. refers to a vertex that does not exist) throws an error.
singleTargets :: (V.NFData a, V.Var a, V.Binary a, V.Typeable a, H.Hashable a, MonadIO m)
    => Implementation -> D.DrawSettings -> G.Graph a Double -> [ST.Query a] -> m (m [[Double]])

singleTargets Baseline _ graph qs = do
    -- We perform arc reversal during setup as we want to compare the baseline and
    -- localcomputation algorithms as if both were single source or single target.
    -- We add zero cost edges to make sure both algorithms would return the same result.
    let reversedGraph = (G.addSelfLoops 0 . G.flipArcDirections) graph

    reversedGraph `deepseq` pure $ do
        -- Can't deepseq H.Graph
        let g = H.fromGraph reversedGraph

        -- Compute solutions
        pure $ map (\q -> H.singleSource g q.target q.sources infinity) qs

singleTargets mode s g qs = pure $ mapM (\q -> fromRight $ algorithm s g q) qs
    where
        algorithm = case mode of
                        (Generic m)  -> ST.singleTarget m
                        (DynamicP _) -> ST.singleTargetDP

--------------------------------------------------------------------------------
-- Variants of `singleTargets`
--------------------------------------------------------------------------------
-- | Multiple problem variant of `singleTargets`. Not to be confused with `singleTargetsSplit` which
-- handles multiple graphs but considers it as one problem.
multipleSingleTargets :: (V.NFData a, V.Var a, V.Binary a, V.Typeable a, H.Hashable a, MonadIO m)
    => Implementation -> D.DrawSettings -> [D.BenchmarkProblem a] -> m (m [[[Double]]])
multipleSingleTargets mode s ps = fmap sequence $ mapM (\p -> singleTargets mode s p.g p.qs) ps

-- | Single query variant of `singleTargets`
singleTarget :: (V.NFData a, V.Var a, V.Binary a, V.Typeable a, H.Hashable a, MonadIO m)
    => Implementation -> D.DrawSettings -> G.Graph a Double -> ST.Query a -> m (m [Double])
singleTarget mode s g q = fmap (fmap head) $ singleTargets mode s g [q]

-- | Variant of `singleTargets` that does the computation in "one go"
-- (doesn't seperate the computation into the 'setup' and 'computation' phases)
singleTargets' :: (V.NFData a, V.Var a, V.Binary a, V.Typeable a, H.Hashable a, MonadIO m)
    => Implementation -> D.DrawSettings -> G.Graph a Double -> [ST.Query a] -> m [[Double]]
singleTargets' mode s g qs = M.join $ singleTargets mode s g qs

-- | Single query variant of `singleTargets'`
singleTarget' :: (V.NFData a, V.Var a, V.Binary a, V.Typeable a, H.Hashable a, MonadIO m)
    => Implementation -> D.DrawSettings -> G.Graph a Double -> ST.Query a -> m [Double]
singleTarget' mode s g q = M.join $ singleTarget mode s g q

--------------------------------------------------------------------------------
-- Split variants of `singleTargets`
--------------------------------------------------------------------------------
-- | Variant of `singleTargets` that takes a graph that has been split across multiple graphs.
singleTargetsSplit :: (V.NFData a, V.Var a, V.Binary a, V.Typeable a, H.Hashable a, MonadIO m)
    => Implementation -> D.DrawSettings -> [G.Graph a Double] -> [ST.Query a] -> m (m [[Double]])

singleTargetsSplit Baseline _ gs qs = do
    -- We perform arc reversal during setup as we want to compare the baseline and
    -- localcomputation algorithms as if both were single source or single target.
    -- We add zero cost edges to make sure both algorithms would return the same result.
    let reversedGraphs = map (G.addSelfLoops 0 . G.flipArcDirections) gs

    reversedGraphs `deepseq` pure $ do
        -- We do count merging as a cost of using the package.
        let g = H.merges H.empty (map H.fromGraph reversedGraphs)

        -- Compute solutions
        pure $ map (\q -> H.singleSource g q.target q.sources infinity) qs

singleTargetsSplit mode s gs qs = pure $ mapM (\q -> fromRight $ algorithm s gs q) qs
    where
        algorithm = case mode of
                        (Generic m)  -> ST.singleTargetSplit m
                        (DynamicP _) -> ST.singleTargetSplitDP

-- | Single query variant of `singleTargetsSplit`
singleTargetSplit :: (V.NFData a, V.Var a, V.Binary a, V.Typeable a, H.Hashable a, MonadIO m)
    => Implementation -> D.DrawSettings -> [G.Graph a Double] -> ST.Query a -> m (m [Double])
singleTargetSplit mode s gs q = fmap (fmap head) $ singleTargetsSplit mode s gs [q]

-- | Variant of `singleTargetsSplit` that does the computation in "one go"
-- (doesn't seperate the computation into the 'setup' and 'computation' phases)
singleTargetsSplit' :: (V.NFData a, V.Var a, V.Binary a, V.Typeable a, H.Hashable a, MonadIO m)
    => Implementation -> D.DrawSettings -> [G.Graph a Double] -> [ST.Query a] -> m [[Double]]
singleTargetsSplit' mode s gs qs = M.join $ singleTargetsSplit mode s gs qs

-- | Single query variant of `singleTargetsSplit'`
singleTargetSplit' :: (V.NFData a, V.Var a, V.Binary a, V.Typeable a, H.Hashable a, MonadIO m)
    => Implementation -> D.DrawSettings -> [G.Graph a Double] -> ST.Query a -> m [Double]
singleTargetSplit' mode s gs q = M.join $ singleTargetSplit mode s gs q


