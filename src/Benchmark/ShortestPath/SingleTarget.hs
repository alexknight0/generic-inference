module Benchmark.ShortestPath.SingleTarget
    (
        benchmarks
    )
where

-- TODO: don't want a dependency of benchmark on test - move queries to a place inside benchmark.
import qualified Benchmark.Data.ShortestPath                          as D

import qualified Benchmark.Baseline.DjikstraSimple                    as H
import           Benchmark.Baseline.FFT                               (dft)
import           Control.Monad.IO.Class                               (liftIO)
import           Criterion.Main
import           Criterion.Types                                      (Config (resamples, timeLimit))
import qualified Data.Map.Lazy                                        as M
import           Debug.Pretty.Simple                                  (pTraceShow,
                                                                       pTraceShowId)
import           LocalComputation.Graph                               (nonSymmetricEdges)
import qualified LocalComputation.Instances.BayesianNetwork           as B
import qualified LocalComputation.Instances.BayesianNetwork.Parser    as P
import           LocalComputation.Instances.FastFourierTransform      (query)
import qualified LocalComputation.Instances.ShortestPath.SingleTarget as ST
import           LocalComputation.LocalProcess                        (run,
                                                                       runProcessLocal)
import           LocalComputation.Utils                               (fromRight,
                                                                       parseFile)
import           LocalComputation.ValuationAlgebra.QuasiRegular       (TropicalSemiringValue (T))
import qualified Tests.BayesianNetwork.Data                           as B
import           Text.Pretty.Simple                                   (pPrint)



benchmarks :: IO ()
benchmarks = do
    p3Small <- D.p3SmallGraph'
    -- pPrint p3Small
    -- results <- runProcessLocal $ ST.singleTarget [fmap T p3Small] [68] 69
    -- pPrint results

    defaultMain [
            --   bgroup "Fast_Fourier_Transform" [
            --           bench "localcomputation" $ nfIO $ runProcessLocal $ query fftInput [0 .. (fromIntegral $ length fftInput - 1)]
            --         , bench "FFTW"             $ nfIO $ dft fftInput
            --     ]
            -- , bgroup "Shortest_Path___P1_Graph" [
            --           bench "localcomputation" $ nfIO (runProcessLocal $ ST.singleTarget [D.p1Graph] D.p1Queries.sources D.p1Queries.target)
            --         , bench "djikstra"         $ nf (H.singleTarget D.p1Graph D.p1Queries.sources D.p1Queries.target) (T $ read "Infinity")
            --     ]
            --   bgroup "Shortest_Path___Small_Graph" [
            --           bench "localcomputation" $ nfIO (runProcessLocal $ ST.singleTarget [fmap T p3Small] [1, 13, 48] 2)
            --         , bench "djikstra"         $ nf (H.singleTarget p3Small [1, 13, 48] 2) (read "Infinity")
            --     ]
        ]

    where
        fftInput = take 100000 $ repeat 0


-- --    defaultMainWith customConfig [
-- bayesianNetwork = fmap fromRight $ parseFile P.network B.asiaFilepath
-- bayesianQuery = (M.fromList [("either", True)], M.fromList [("asia", True), ("tub", False), ("smoke", False), ("bronc", True)])
--
-- bayesian n = run $ do
--     result <- B.getProbability [bayesianQuery] n
--     -- pTraceShow result (pure result)
--     pure result
--
-- customConfig :: Config
-- customConfig = defaultConfig
--   { resamples = 10000       -- Number of bootstrap resamples (default is 1000)
--   , timeLimit = 100
--   }
--


