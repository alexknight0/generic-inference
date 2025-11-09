{-# LANGUAGE CPP #-}
module Benchmarks.ShortestDistance (
      benchmarkPerformance
    , benchmarkComplexity
    , justDraw
    , singleTarget
    , singleTargets
    , singleTarget'
    , singleTargets'
    , singleTargetSplit
    , singleTargetsSplit
    , singleTargetSplit'
    , singleTargetsSplit'
    , multipleSingleTargetsSplit
    , Implementation (..)
    , allImplementations
    , allButBaseline
) where

import qualified Benchmarks.ShortestDistance.Data           as D

import qualified Benchmarks.ShortestDistance.Baseline       as H
import qualified Benchmarks.Utils                                    as U
import           Control.DeepSeq                                     (deepseq,
                                                                      rnf)
import           Control.Exception.Base                              (evaluate)
import qualified Control.Monad                                       as M
import           Control.Monad.Extra                                 (concatMapM)
import           Control.Monad.IO.Class                              (MonadIO,
                                                                      liftIO)
import           Criterion.Main
import           Data.Function                                       ((&))
import qualified Data.Hashable                                       as H
import qualified Data.List                                           as L
import qualified Data.Time.Clock.POSIX                               as C
import qualified GenericInference.Graph                              as G
import qualified GenericInference.Inference                          as I
import qualified GenericInference.Inference.JoinTree.Diagram         as D
import qualified GenericInference.Inference.MessagePassing           as MP
import qualified GenericInference.Inference.Statistics               as S
import qualified GenericInference.Problems.ShortestDistance as ST
import           GenericInference.Utils                              (fromRight,
                                                                      infinity)
import qualified GenericInference.ValuationAlgebra                   as V
import           Numeric.Natural

--------------------------------------------------------------------------------
-- Benchmarks
--------------------------------------------------------------------------------
justDraw :: (MonadIO m) => m ()
justDraw = do
    p <- fmap (head . (.ps) . head) $ setProblems

    _ <- fromRight $ ST.singleTarget (I.Shenoy MP.Threads) draw p.g (head p.qs)

    pure ()

    where
        draw = D.def { D.beforeInference = Just "diagrams/tmp/testing.svg" }

-- The graph generation is not part of the benchmark (we don't want it to take up time!), so we can't
-- have it use different graphs between benchmarks unless we created some 'state', pregenerated all the
-- graphs it was going to use (we don't know how many graphs that will be) and then swapped between those
-- based on this state (where the state is hidden by the IO)
setProblems :: (MonadIO m) => m [D.BenchmarkProblem Natural]
setProblems = sequence $ zipWith (&) seeds $ map (\edges -> const $ D.newYorkProblemOneToOne 10 edges) edgesToTest

    where
        -- Num edges to pull from the new york problem which will be tested.
        edgesToTest = [100]
        seeds :: [Int]
        seeds = [0..]


setModes :: [Implementation]
setModes = [
      Baseline
    , Generic  $ I.BruteForce
    , Generic  $ I.Fusion
    , Generic  $ I.Shenoy MP.Threads
    , Generic  $ I.Shenoy MP.Distributed
    , DynamicP $ MP.Threads
    , DynamicP $ MP.Distributed
  ]


createHeader :: Integer -> D.BenchmarkProblem a -> Implementation -> [String]
createHeader timestamp p mode = [show timestamp
                               , "Shortest Path"
                               ,      p.name
                               , show p.numProblems
                               , show p.numQueries
                               , show p.numVertices
                               , show p.edgeRatio
                               ,      seed
                               , implementationName mode
                            ]
    where
        seed = case p.seed of
                Nothing -> "No seed"
                Just s  -> show s


--------------------------------------------------------------------------------
-- Complexity Benchmarking
--------------------------------------------------------------------------------
benchmarkComplexity :: IO ()
benchmarkComplexity = do
    timestamp <- fmap round C.getPOSIXTime :: IO Integer

    ps <- setProblems

    mapM_ (benchComplexityOnModes timestamp setModes) ps

    where
        benchComplexityOnModes timestamp modes p = mapM_ (\m -> benchComplexityOnMode timestamp m p) modes


benchComplexityOnMode :: (V.NFData a, Show a, Ord a, V.Binary a, V.Typeable a, H.Hashable a)
    => Integer
    -> Implementation
    -> D.BenchmarkProblem a
    -> IO ()
benchComplexityOnMode timestamp mode p = liftIO $ U.benchmarkComplexity header problem complexity
    where
        header = createHeader timestamp p mode

        problem = M.join $ multipleSingleTargets mode D.def p

        complexity = case mode of
                        Generic (I.Fusion)     -> fusionComplexity
                        DynamicP (_)           -> fusionComplexity
                        Generic (I.Shenoy (_)) -> const 0  -- Not yet implemented.
                        _                      -> const 0  -- Not yet implemented.


fusionComplexity :: S.Stats -> Int
fusionComplexity s = sum $ zipWith f s.valuations s.treeWidths
    where
        f m w = m * square (w + 1)

        square x = x * x

--------------------------------------------------------------------------------
-- Performance Testing
--------------------------------------------------------------------------------
benchmarkPerformance :: IO [Benchmark]
benchmarkPerformance = do
    timestamp <- fmap round C.getPOSIXTime :: IO Integer

    ps <- setProblems
    evaluate (rnf ps)

    benches <- concatMapM (benchModes timestamp setModes) ps

    pure $ benches


benchModes :: (V.NFData a, Show a, Ord a, V.Binary a, V.Typeable a, H.Hashable a)
    => Integer
    -> [Implementation]
    -> D.BenchmarkProblem a
    -> IO [Benchmark]
benchModes timestamp modes p = mapM benchMode modes
    where
        header mode = L.intercalate "/" $ createHeader timestamp p mode

        benchMode mode = do
            afterSetup <- multipleSingleTargets mode D.def p
            pure $ bench (header mode) $ nfIO afterSetup

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

implementationName :: Implementation -> String
implementationName (Baseline)                          = "Djikstra's Algorithm"
implementationName (Generic I.BruteForce)              = "Brute Force"
implementationName (Generic I.Fusion)                  = "Fusion"
implementationName (Generic (I.Shenoy MP.Threads))     = "Shenoy (threads)"
implementationName (Generic (I.Shenoy MP.Distributed)) = "Shenoy (distributed)"
implementationName (DynamicP (MP.Threads))             = "Dynamic Programming (threads)"
implementationName (DynamicP (MP.Distributed))         = "Dynamic Programming (distributed)"

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
    => Implementation -> D.DrawSettings -> G.Graph a Double -> [ST.Query a] -> m (m (S.WithStats [[Double]]))

singleTargets Baseline _ graph qs = do
    -- We perform arc reversal during setup as we want to compare the baseline and
    -- localcomputation algorithms as if both were single source or single target.
    -- We add zero cost edges to make sure both algorithms would return the same result.
    let reversedGraph = (G.addSelfLoops 0 . G.flipArcDirections) graph

    reversedGraph `deepseq` pure $ do
        -- Can't deepseq H.Graph
        let g = H.fromGraph reversedGraph

        -- Compute solutions
        pure $ S.withNoStats $ map (\q -> H.singleSource g q.target q.sources infinity) qs

singleTargets mode s g qs = pure $ fmap S.lift $ mapM (\q -> fromRight $ algorithm s g q) qs
    where
        algorithm = case mode of
                        (Generic m)  -> ST.singleTarget m
                        (DynamicP m) -> ST.singleTargetDP m

--------------------------------------------------------------------------------
-- Variants of `singleTargets`
--------------------------------------------------------------------------------
-- | Multiple problem variant of `singleTargets`. Not to be confused with `singleTargetsSplit` which
-- handles multiple graphs but considers it as one problem.
multipleSingleTargets :: (V.NFData a, V.Var a, V.Binary a, V.Typeable a, H.Hashable a, MonadIO m)
    => Implementation -> D.DrawSettings -> D.BenchmarkProblem a -> m (m (S.WithStats [[[Double]]]))
multipleSingleTargets mode s ps = fmap (fmap S.lift . sequence) $ mapM (\p -> singleTargets mode s p.g p.qs) ps.ps

-- | Single query variant of `singleTargets`
singleTarget :: (V.NFData a, V.Var a, V.Binary a, V.Typeable a, H.Hashable a, MonadIO m)
    => Implementation -> D.DrawSettings -> G.Graph a Double -> ST.Query a -> m (m (S.WithStats [Double]))
singleTarget mode s g q = fmap (fmap (fmap head)) $ singleTargets mode s g [q]

-- | Variant of `singleTargets` that does the computation in "one go"
-- (doesn't seperate the computation into the 'setup' and 'computation' phases)
singleTargets' :: (V.NFData a, V.Var a, V.Binary a, V.Typeable a, H.Hashable a, MonadIO m)
    => Implementation -> D.DrawSettings -> G.Graph a Double -> [ST.Query a] -> m (S.WithStats [[Double]])
singleTargets' mode s g qs = M.join $ singleTargets mode s g qs

-- | Single query variant of `singleTargets'`
singleTarget' :: (V.NFData a, V.Var a, V.Binary a, V.Typeable a, H.Hashable a, MonadIO m)
    => Implementation -> D.DrawSettings -> G.Graph a Double -> ST.Query a -> m (S.WithStats [Double])
singleTarget' mode s g q = M.join $ singleTarget mode s g q

--------------------------------------------------------------------------------
-- Split variants of `singleTargets`
--------------------------------------------------------------------------------
-- | Multiple problem variant of `singleTargets`. Not to be confused with `singleTargetsSplit` which
-- handles multiple graphs but considers it as one problem.
multipleSingleTargetsSplit :: (V.NFData a, V.Var a, V.Binary a, V.Typeable a, H.Hashable a, MonadIO m)
    => Implementation -> D.DrawSettings -> D.BenchmarkProblem a -> m (m (S.WithStats [[[Double]]]))
multipleSingleTargetsSplit mode s ps = fmap (fmap S.lift . sequence) $ mapM (\p -> singleTargetsSplit mode s [p.g] p.qs) ps.ps

-- | Variant of `singleTargets` that takes a graph that has been split across multiple graphs.
singleTargetsSplit :: (V.NFData a, V.Var a, V.Binary a, V.Typeable a, H.Hashable a, MonadIO m)
    => Implementation -> D.DrawSettings -> [G.Graph a Double] -> [ST.Query a] -> m (m (S.WithStats [[Double]]))

singleTargetsSplit Baseline _ gs qs = do
    -- We perform arc reversal during setup as we want to compare the baseline and
    -- localcomputation algorithms as if both were single source or single target.
    -- We add zero cost edges to make sure both algorithms would return the same result.
    let reversedGraphs = map (G.addSelfLoops 0 . G.flipArcDirections) gs

    reversedGraphs `deepseq` pure $ do
        -- We do count merging as a cost of using the package.
        let g = H.merges H.empty (map H.fromGraph reversedGraphs)

        -- Compute solutions
        pure $ S.withNoStats $ map (\q -> H.singleSource g q.target q.sources infinity) qs

singleTargetsSplit mode s gs qs = pure $ fmap S.lift $ mapM (\q -> fromRight $ algorithm s gs q) qs
    where
        algorithm = case mode of
                        (Generic m)  -> ST.singleTargetSplit m
                        (DynamicP m) -> ST.singleTargetSplitDP m

-- | Single query variant of `singleTargetsSplit`
singleTargetSplit :: (V.NFData a, V.Var a, V.Binary a, V.Typeable a, H.Hashable a, MonadIO m)
    => Implementation -> D.DrawSettings -> [G.Graph a Double] -> ST.Query a -> m (m (S.WithStats [Double]))
singleTargetSplit mode s gs q = fmap (fmap (fmap head)) $ singleTargetsSplit mode s gs [q]

-- | Variant of `singleTargetsSplit` that does the computation in "one go"
-- (doesn't seperate the computation into the 'setup' and 'computation' phases)
singleTargetsSplit' :: (V.NFData a, V.Var a, V.Binary a, V.Typeable a, H.Hashable a, MonadIO m)
    => Implementation -> D.DrawSettings -> [G.Graph a Double] -> [ST.Query a] -> m (S.WithStats [[Double]])
singleTargetsSplit' mode s gs qs = M.join $ singleTargetsSplit mode s gs qs

-- | Single query variant of `singleTargetsSplit'`
singleTargetSplit' :: (V.NFData a, V.Var a, V.Binary a, V.Typeable a, H.Hashable a, MonadIO m)
    => Implementation -> D.DrawSettings -> [G.Graph a Double] -> ST.Query a -> m (S.WithStats [Double])
singleTargetSplit' mode s gs q = M.join $ singleTargetSplit mode s gs q

