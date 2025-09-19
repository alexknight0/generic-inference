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
import           Criterion.Main
import qualified LocalComputation.Inference                        as I
import qualified LocalComputation.Inference.JoinTree.Diagram       as D
import qualified LocalComputation.Instances.BayesianNetwork        as BN
import qualified LocalComputation.Instances.BayesianNetwork.Parser as P
import qualified LocalComputation.LocalProcess                     as P
import qualified LocalComputation.Utils                            as U

data WithName a = WithName {
      name  :: String
    , value :: a
}

benchmarks :: IO [Benchmark]
benchmarks = do
        smallNet  <- fmap (WithName "SmallNet") $ U.unsafeParseFile' P.network D.asiaFilepath
        mediumNet <- fmap (WithName "MediumNet") $ U.unsafeParseFile' P.network D.alarmFilepath

        sequence [
                   benchmark smallNet  $ WithName "ManyEasyQueries"     $ D.genQueries      smallNet.value  20 1 0
                 , benchmark smallNet  $ WithName "ManyMediumQueries"   $ D.genQueries      smallNet.value  20 4 10
                 , benchmark smallNet  $ WithName "ManyHardQueries"     $ D.genQueries      smallNet.value  20 4 40
                 , benchmark mediumNet $ WithName "ManyEasyQueries"     $ D.genQueries      mediumNet.value 20 1 0
                 , benchmark mediumNet $ WithName "SomeMediumQueries"   $ D.genQueriesExact mediumNet.value  1 1 1
                ]


benchmark :: WithName (BN.Network String String) -> WithName (D.Gen [BN.Query String String]) -> IO Benchmark
benchmark net queryGen = do
    queries <- D.sample queryGen.value

    pure $ bgroup ("Bayesian/" ++ net.name ++ "/" ++ queryGen.name) [
                  bench "localcomputation-current"   $ nfIO $ P.run $ BN.getProbability I.BruteForce D.def queries net.value
            ]
