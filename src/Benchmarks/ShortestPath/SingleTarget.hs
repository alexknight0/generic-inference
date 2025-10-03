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
-- And after:
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

import           Control.DeepSeq                                      (deepseq,
                                                                       rnf)
import           Control.Exception.Base                               (evaluate)
import qualified Control.Monad                                        as M
import           Control.Monad.IO.Class                               (MonadIO)
import           Data.Function                                        ((&))
import qualified Data.Hashable                                        as H
import qualified LocalComputation.Inference.JoinTree.Diagram          as D
import qualified LocalComputation.Inference.MessagePassing            as MP
import qualified LocalComputation.Utils                               as U
import qualified LocalComputation.ValuationAlgebra                    as V
import           System.IO                                            (hPutStrLn,
                                                                       stderr)

--------------------------------------------------------------------------------
-- Benchmarks
--------------------------------------------------------------------------------

-- TODO: test graph composition diff.
-- TODO: filter by trees with query for message passing

-- The graph generation is not part of the benchmark (we don't want it to take up time!), so we can't
-- have it use different graphs between benchmarks unless we created some 'state', pregenerated all the
-- graphs it was going to use (we don't know how many graphs that will be) and then swapped between those
-- based on this state (where the state is hidden by the IO)

benchmarks :: IO [Benchmark]
benchmarks = do

    problems <- sequence $ zipWith (&) seeds [
                                              D.createRandomProblem 1 1 35 4
                                              -- take 10 $ repeat $ D.genProblem 50 100 1
                                                  -- take 1 $ repeat $ D.genProblem 50 250 1
                                                  -- , take 10 $ repeat $ D.genProblem 50 500 1
                                                  -- , take 10 $ repeat $ D.genProblem 100 500 1
                                                  -- , take 10 $ repeat $ D.genProblem 100 2000 1
                                                  -- take 3 $ repeat $ D.genProblem 200 1000 1
                                                  -- D.genProblem 200 4000 1
                                                  -- take 1 $ repeat $ D.genProblem 50 100 1
                                                  -- , take 1 $ repeat $ D.genProblem 400 8000 1
                                             ]
    evaluate (rnf problems)

    benches <- mapM (benchModes modes) problems

    pure $ pure $ bgroup "Shortest Path" $ benches
    where
        seeds = [0..]

        modes = [
            -- Baseline
              Generic  $ I.Fusion
            -- , Generic  $ I.Shenoy MP.Threads
            -- , Generic  $ I.Shenoy MP.Distributed
            -- , DynamicP $ MP.Distributed
            -- , DynamicP $ MP.Threads
         ]


benchModes :: (V.NFData a, Show a, Ord a, V.Binary a, V.Typeable a, H.Hashable a)
    => [Implementation]
    -> D.BenchmarkProblem a
    -> IO Benchmark
benchModes modes p = fmap (bgroup name) $ mapM (\m -> benchMode m p) modes
    where
        name =        show p.numProblems
            ++ "/" ++ show p.numQueries
            ++ "/" ++ show p.numVertices
            ++ "/" ++ show p.edgeRatio

benchMode :: (V.NFData a, Show a, Ord a, V.Binary a, V.Typeable a, H.Hashable a)
    => Implementation
    -> D.BenchmarkProblem a
    -> IO Benchmark
benchMode mode p = do
    -- TODO: if we were using split why was decomposition slowing it down... Wait that means decomposition was
    -- taking a while no?? Wait no it literally just should not have affected it - it never would have been called...
    afterSetup <- multipleSingleTargets mode D.def p
    pure $ bench name $ nfIO afterSetup

    where
        name = implementationName mode

implementationName :: Implementation -> String
implementationName (Baseline)                          = "Djikstra's Algorithm"
implementationName (Generic I.BruteForce)              = "Brute Force"
implementationName (Generic I.Fusion)                  = "Fusion"
implementationName (Generic (I.Shenoy MP.Threads))     = "Shenoy (threads)"
implementationName (Generic (I.Shenoy MP.Distributed)) = "Shenoy (distributed)"
implementationName (DynamicP (MP.Threads))             = "Dynamic Programming (threads)"
implementationName (DynamicP (MP.Distributed))         = "Dynamic Programming (distributed)"


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
                     , DynamicP $ MP.Threads
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
                        (DynamicP m) -> ST.singleTargetDP m

        --TODO: remove
        -- debug = U.debugWithCounter f
        --     where
        --         f count v
        --             | Left e <- v, count == 1 = do hPutStrLn stderr (show e); hPutStrLn stderr str; pure True
        --             | otherwise = pure False
        --
        --             where
        --                 str = "\nBefore decomposition:" ++ show g
        --                     ++ "\nAfter decomposition:" ++ show (ST.decomposition g)
        --                     ++ "\n" ++ show qs

--------------------------------------------------------------------------------
-- Variants of `singleTargets`
--------------------------------------------------------------------------------
-- | Multiple problem variant of `singleTargets`. Not to be confused with `singleTargetsSplit` which
-- handles multiple graphs but considers it as one problem.
multipleSingleTargets :: (V.NFData a, V.Var a, V.Binary a, V.Typeable a, H.Hashable a, MonadIO m)
    => Implementation -> D.DrawSettings -> D.BenchmarkProblem a -> m (m [[[Double]]])
multipleSingleTargets mode s ps = fmap sequence $ mapM (\p -> singleTargets mode s p.g p.qs) ps.ps

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
-- | Multiple problem variant of `singleTargets`. Not to be confused with `singleTargetsSplit` which
-- handles multiple graphs but considers it as one problem.
multipleSingleTargetsSplit :: (V.NFData a, V.Var a, V.Binary a, V.Typeable a, H.Hashable a, MonadIO m)
    => Implementation -> D.DrawSettings -> D.BenchmarkProblem a -> m (m [[[Double]]])
multipleSingleTargetsSplit mode s ps = fmap sequence $ mapM (\p -> singleTargetsSplit mode s [p.g] p.qs) ps.ps

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
                        (DynamicP m) -> ST.singleTargetSplitDP m

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


