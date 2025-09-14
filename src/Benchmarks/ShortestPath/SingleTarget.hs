module Benchmarks.ShortestPath.SingleTarget (
      benchmarks
    , singleTarget
    , singleTargets
    , singleTarget'
    , singleTargets'
    , Implementation (..)
    , allImplementations
    , allButBaseline
) where

-- TODO: !!!!!!!!!!!!!!!! BEFORE BENCHMARKING !!!!!!!!!!!!!!!!!!!!!
-- TODO: !!!!!!!!!!!!!!!! BEFORE BENCHMARKING !!!!!!!!!!!!!!!!!!!!!
-- TODO: !!!!!!!!!!!!!!!! BEFORE BENCHMARKING !!!!!!!!!!!!!!!!!!!!!
-- 1. Make fusion construct a better join tree for itself.
-- 2. Make quasiregular split the graph nicely for itself.
-- And after:
-- 1. Test singleTarget with a multiple query architecture, splitting
--    the 'domain' into pairs of (target, source) instead of a single
--    large query.

import qualified Benchmarks.ShortestPath.SingleTarget.Data            as D

import qualified Benchmarks.ShortestPath.SingleTarget.Baseline        as H
import           Benchmarks.ShortestPath.SingleTarget.Data            (genConnectedQueries,
                                                                       sample)
import           Criterion.Main
import qualified LocalComputation.Graph                               as G
import qualified LocalComputation.Inference                           as I
import qualified LocalComputation.Instances.ShortestPath.SingleTarget as ST
import           LocalComputation.Utils                               (fromRight,
                                                                       infinity)

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
    -- p3Small <- D.p3SmallGraph'
    graph <- sample $ D.genGraph 50 200
    -- p3Medium <- D.p3MediumGraph'
    -- tmp <- sample $ genConnectedQueries 10 p3Medium
    queries <- sample $ genConnectedQueries 10 graph

    bruteForce  <- singleTargets (Generic I.BruteForce)          D.def [graph] queries
    fusion      <- singleTargets (Generic I.Fusion)              D.def [graph] queries
    shenoy      <- singleTargets (Generic (I.Shenoy MP.Threads)) D.def [graph] queries
    baseline    <- singleTargets (Baseline)                      D.def [graph] queries

    pure $ pure $ bgroup "Shortest Path" [
                  bench "localcomputation-bruteForce" $ nfIO bruteForce
                , bench "localcomputation-fusion"     $ nfIO fusion
                , bench "localcomputation-shenoy"     $ nfIO shenoy
                , bench "djikstra"                    $ nfIO baseline
            ]

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
-- Takes a graph that has been split across multiple graphs.
--
-- Takes multiple queries. No single target implementation currently supports solving multiple queries
-- faster than solving a single one multiple times; but by taking multiple queries we can benchmark
-- a function on the same graph without recreating the graph.
--
-- Technically unsafe - if one of the given queries is not answerable for the problem, will
-- (i.e. refers to a vertex that does not exist) throws an error.
singleTargets :: (V.NFData a, V.Var a, V.Binary a, V.Typeable a, H.Hashable a, MonadIO m)
    => Implementation -> D.DrawSettings -> [G.Graph a Double] -> [ST.Query a] -> m (m [[Double]])

singleTargets Baseline _ gs qs = do
    -- We perform arc reversal during setup as we want to compare the baseline and
    -- localcomputation algorithms as if both were single source or single target.
    -- We add zero cost edges to make sure both algorithms would return the same result.
    let reversedGraphs = map (G.addSelfLoops 0 . G.flipArcDirections) gs

    reversedGraphs `deepseq` pure $ do
        -- We do count merging as a cost of using the package.
        let g = H.merges H.empty (map H.fromGraph reversedGraphs)

        -- Compute solutions
        pure $ map (\q -> H.singleSource g q.target q.sources infinity) qs

singleTargets mode s gs qs = pure $ mapM (\q -> fromRight $ algorithm s gs q) qs
    where
        algorithm = case mode of
                        (Generic m)  -> ST.singleTarget m
                        (DynamicP _) -> ST.singleTargetDP

--------------------------------------------------------------------------------
-- Variants of `singleTargets`
--------------------------------------------------------------------------------

-- | Single query variant of `singleTargets`
singleTarget :: (V.NFData a, V.Var a, V.Binary a, V.Typeable a, H.Hashable a, MonadIO m)
    => Implementation -> D.DrawSettings -> [G.Graph a Double] -> ST.Query a -> m (m [Double])
singleTarget mode s gs q = fmap (fmap head) $ singleTargets mode s gs [q]

-- | Variant of `singleTargets` that does the computation in "one go"
-- (doesn't seperate the computation into the 'setup' and 'computation' phases)
singleTargets' :: (V.NFData a, V.Var a, V.Binary a, V.Typeable a, H.Hashable a, MonadIO m)
    => Implementation -> D.DrawSettings -> [G.Graph a Double] -> [ST.Query a] -> m [[Double]]
singleTargets' mode s gs qs = M.join $ singleTargets mode s gs qs

-- | Single query variant of `singleTargets'`
singleTarget' :: (V.NFData a, V.Var a, V.Binary a, V.Typeable a, H.Hashable a, MonadIO m)
    => Implementation -> D.DrawSettings -> [G.Graph a Double] -> ST.Query a -> m [Double]
singleTarget' mode s gs q = M.join $ singleTarget mode s gs q


