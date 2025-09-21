module Benchmarks.BayesianNetwork
    (
        benchmarks
    )
where

-- TODO: !!!!!!!!!!!!!!!! BEFORE BENCHMARKING !!!!!!!!!!!!!!!!!!!!!
-- TODO: !!!!!!!!!!!!!!!! BEFORE BENCHMARKING !!!!!!!!!!!!!!!!!!!!!
-- TODO: !!!!!!!!!!!!!!!! BEFORE BENCHMARKING !!!!!!!!!!!!!!!!!!!!!
-- 1. Make fusion construct a better join tree for itself.
-- 2. Make quasiregular split the graph nicely for itself.
-- And after:
-- 1. Test singleTarget with a multiple query architecture, splitting
--    the 'domain' into pairs of (target, source) instead of a single
--    large query.

import qualified Benchmarks.BayesianNetwork.Data                   as D
import           Control.Monad.IO.Class                            (MonadIO)
import           Criterion.Main
import qualified LocalComputation.Inference                        as I
import qualified LocalComputation.Inference.JoinTree.Diagram       as D
import qualified LocalComputation.Inference.MessagePassing         as MP
import qualified LocalComputation.Instances.BayesianNetwork        as BN
import qualified LocalComputation.Instances.BayesianNetwork.Parser as P
import qualified LocalComputation.LocalProcess                     as P
import qualified LocalComputation.Utils                            as U

data WithName a = WithName {
      name  :: String
    , value :: a
}

data Problem    = Problem    { net :: BN.Network String String, qs    ::       [BN.Query String String] }
data GenProblem = GenProblem { net :: BN.Network String String, qsGen :: D.Gen [BN.Query String String] }

benchmarks :: IO [Benchmark]
benchmarks = do
        smallNet  <- U.unsafeParseFile' P.network D.asiaFilepath
        mediumNet <- U.unsafeParseFile' P.network D.alarmFilepath

        problems <- sequence $ [ -- createProblem smallNet 10 1 0
                               -- , createProblem smallNet 20 4 10
                               -- , createProblem smallNet  20 4 40
                               -- createProblem mediumNet 20 1 0
                                 createProblem mediumNet 3  1 1
                               , createProblem mediumNet 3  1 1
                               , createProblem mediumNet 3  1 1
                               , createProblem mediumNet 3  1 1

                               , createProblem mediumNet 3  1 1
                               , createProblem mediumNet 3  1 1
                               , createProblem mediumNet 3  1 1
                               , createProblem mediumNet 3  1 1

                               , createProblem mediumNet 3  1 1
                               , createProblem mediumNet 3  1 1
                               , createProblem mediumNet 3  1 1
                               , createProblem mediumNet 3  1 1

                               , createProblem mediumNet 3  1 1
                               , createProblem mediumNet 3  1 1
                               , createProblem mediumNet 3  1 1
                               , createProblem mediumNet 3  1 1

                               , createProblem mediumNet 3  1 1
                               , createProblem mediumNet 3  1 1
                               , createProblem mediumNet 3  1 1
                               , createProblem mediumNet 3  1 1

                               , createProblem mediumNet 3  1 1
                               , createProblem mediumNet 3  1 1
                               , createProblem mediumNet 3  1 1
                               , createProblem mediumNet 3  1 1
                              ]

        let algorithm mode = bench ("localcomputation-" ++ show mode) $ nfIO $ multipleGetProbability mode problems

        pure $ pure $ bgroup ("Bayesian") $ map algorithm [
                                                            -- I.Fusion
                                                          I.Shenoy MP.Threads
                                                          -- , I.Shenoy MP.Distributed
                                                         ]

createProblem :: (MonadIO m) => BN.Network String String -> Int -> Int -> Int -> m Problem
createProblem net numQueries maxConditioned maxConditional = do
    qs <- D.sample $ D.genQueries net numQueries maxConditioned maxConditional
    pure $ Problem net qs

multipleGetProbability :: (MonadIO m) => I.Mode -> [Problem] -> m [[BN.Probability]]
multipleGetProbability mode ps = mapM (\p -> BN.getProbability mode D.def p.qs p.net) ps

benchmark :: WithName (BN.Network String String) -> WithName (D.Gen [BN.Query String String]) -> IO Benchmark
benchmark net queryGen = do
    queries <- D.sample queryGen.value

    pure $ bgroup ("Bayesian/" ++ net.name ++ "/" ++ queryGen.name) [
                  bench "localcomputation-current"   $ nfIO $ P.run $ BN.getProbability I.BruteForce D.def queries net.value
            ]
