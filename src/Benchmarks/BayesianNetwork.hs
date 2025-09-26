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
import qualified Benchmarks.Utils                                  as U
import           Control.Monad.IO.Class                            (MonadIO)
import           Criterion.Main
import           Data.Function                                     ((&))
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

        singleQueryProblems <- sequence $ zipWith (&) seeds $ concat [
                                                       take 5 $ repeat $ createProblem mediumNet 1  1 0
                                                     , take 5 $ repeat $ createProblem mediumNet 1  1 1
                                                     , take 5 $ repeat $ createProblem mediumNet 1  2 1
                                                     , take 5 $ repeat $ createProblem mediumNet 1  2 2
                                                  ]

        multipleQueryProblems <- sequence $ zipWith (&) seeds $ concat [
                                                       take 5 $ repeat $ createProblem mediumNet 5  1 0
                                                     , take 5 $ repeat $ createProblem mediumNet 5  1 1
                                                     , take 5 $ repeat $ createProblem mediumNet 5  2 1
                                                     , take 5 $ repeat $ createProblem mediumNet 5  2 2
                                                  ]

        pure $ [ bgroup ("Bayesian - single query") $ map (benchMode singleQueryProblems) [
                                                            I.Fusion
                                                          , I.Shenoy MP.Threads
                                                          , I.Shenoy MP.Distributed
                                                         ]
               , bgroup ("Bayesian - multiple queries") $ map (benchMode multipleQueryProblems) [
                                                            I.Fusion
                                                          , I.Shenoy MP.Threads
                                                          , I.Shenoy MP.Distributed
                                                         ]
            ]
        where
            seeds = [0..]


            benchMode problems mode = bench (show mode) $ nfIO $ multipleGetProbability mode problems

createProblem :: (MonadIO m) => BN.Network String String -> Int -> Int -> Int -> Int -> m Problem
createProblem net numQueries maxConditioned maxConditional seed = do
    qs <- U.sample (seed) $ D.genQueries net numQueries maxConditioned maxConditional
    pure $ Problem net qs

multipleGetProbability :: (MonadIO m) => I.Mode -> [Problem] -> m [[BN.Probability]]
multipleGetProbability mode ps = mapM (\p -> BN.getProbability mode D.def p.qs p.net) ps

benchmark :: WithName (BN.Network String String) -> WithName (D.Gen [BN.Query String String]) -> IO Benchmark
benchmark net queryGen = do
    queries <- D.sample queryGen.value

    pure $ bgroup ("Bayesian/" ++ net.name ++ "/" ++ queryGen.name) [
                  bench "localcomputation-current"   $ nfIO $ P.run $ BN.getProbability I.BruteForce D.def queries net.value
            ]

foobar = U.getSeed 3
{-

>>> do mediumNet <- U.unsafeParseFile' P.network D.alarmFilepath; U.sample 15 $ D.genQueries mediumNet 1 1 1
[Query {conditioned = fromList [("ERRCAUTER","TRUE")], conditional = fromList [("HR","NORMAL")]}]

-}
