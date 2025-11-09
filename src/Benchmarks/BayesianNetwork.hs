module Benchmarks.BayesianNetwork
    (
          benchmarkPerformance
        , benchmarkComplexity
    )
where

import qualified Benchmarks.BayesianNetwork.Data                   as D
import qualified Benchmarks.Utils                                  as U
import           Control.Monad.IO.Class                            (MonadIO,
                                                                    liftIO)
import           Criterion.Main
import           Data.Function                                     ((&))
import qualified Data.List                                         as L
import qualified Data.Time.Clock.POSIX                             as C
import qualified LocalComputation.Inference                        as I
import qualified LocalComputation.Inference.JoinTree.Diagram       as D
import qualified LocalComputation.Inference.MessagePassing         as MP
import qualified LocalComputation.Inference.Statistics             as S
import qualified LocalComputation.Instances.BayesianNetwork        as BN
import qualified LocalComputation.Instances.BayesianNetwork.Parser as P
import qualified LocalComputation.ValuationAlgebra                 as V

getAsiaNet :: IO (D.NamedNet (BN.Network String String))
getAsiaNet = fmap (D.NamedNet "Asia") $ U.unsafeParseFile' P.network D.alarmFilepath

setProblems :: MonadIO m => m [D.Problems]
setProblems = do
    asiaNet <- liftIO $ getAsiaNet

    sequence $ zipWith (&) seeds $ concat $ map (replicate 1) [
        D.genProblem g 1 q 1 1 | q <- [1], g <- [asiaNet]
      ]

    where
        seeds :: [Int]
        seeds = [0..]


setModes :: [I.Mode]
setModes = [
            I.Fusion
           , I.Shenoy MP.Threads
           , I.Shenoy MP.Distributed
        ]

--------------------------------------------------------------------------------
-- Complexity benchmarking
--------------------------------------------------------------------------------
benchmarkComplexity :: IO ()
benchmarkComplexity = do
    timestamp <- fmap round C.getPOSIXTime :: IO Integer

    ps <- setProblems

    mapM_ (benchComplexityOnModes timestamp setModes) ps

    where
        benchComplexityOnModes timestamp modes p = mapM_ (\m -> benchComplexityOnMode timestamp m p) modes

benchComplexityOnMode ::
    Integer
    -> I.Mode
    -> D.Problems
    -> IO ()
benchComplexityOnMode timestamp mode ps = liftIO $ U.benchmarkComplexity header problem complexity
    where
        header = createHeader timestamp ps mode

        problem = solveProblems ps mode

        complexity = case mode of
                        I.Fusion     -> fusionComplexity
                        I.Shenoy (_) -> binaryShenoyComplexity
                        _            -> const 0

-- | Interprets infinity as 0, as we can't properly calculate a value for infinity.
fusionComplexity :: S.Stats -> Int
fusionComplexity stats = sum $ L.zipWith3 f stats.valuations stats.sumFrameLengths stats.overarchingDomainSize
    where
        f _ V.Infinity _    = 0
        f m (V.Int p)  dPhi = (m + dPhi) * p

-- | Interprets infinity as 0, as we can't properly calculate a value for infinity.
binaryShenoyComplexity :: S.Stats -> Int
binaryShenoyComplexity stats = sum $ zipWith f stats.treeVertices stats.sumFrameLengths
    where
        f _ V.Infinity = 0
        f v (V.Int p)  = v * p


--------------------------------------------------------------------------------
-- Performance benchmarking
--------------------------------------------------------------------------------
benchmarkPerformance :: IO [Benchmark]
benchmarkPerformance = do
    timestamp <- fmap round C.getPOSIXTime :: IO Integer

    ps <- setProblems

    pure $ concatMap (benchModes timestamp) ps
    where
        benchModes timestamp problem = map (benchMode timestamp problem) setModes

        benchMode :: Integer -> D.Problems -> I.Mode -> Benchmark
        benchMode timestamp problems mode = bench header $ nfIO $ solveProblems problems mode
            where
                header = L.intercalate "/" $ createHeader timestamp problems mode


solveProblems :: D.Problems -> I.Mode -> IO (S.WithStats [[BN.Probability]])
solveProblems ps mode = fmap S.lift $ mapM (\qs -> BN.getProbability mode D.def qs ps.net.net) ps.problems

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------
createHeader :: Integer -> D.Problems -> I.Mode -> [String]
createHeader timestamp p mode = [show timestamp
                               , "Bayesian Inference"
                               , p.name ++ " - " ++ p.net.name
                               , show numProblems
                               , show numQueries
                               , show p.numConditioned
                               , show p.numConditional
                               , seed
                               , modeName mode
                            ]

    where

        numProblems = length p.problems
        numQueries = length $ head p.problems

        seed = case p.seed of
                Nothing -> "No seed"
                Just s  -> show s

modeName :: I.Mode -> String
modeName I.BruteForce              = "Brute Force"
modeName I.Fusion                  = "Fusion"
modeName (I.Shenoy MP.Threads)     = "Shenoy (threads)"
modeName (I.Shenoy MP.Distributed) = "Shenoy (distributed)"
