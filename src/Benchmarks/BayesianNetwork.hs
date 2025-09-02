module Benchmarks.BayesianNetwork
    (
        benchmarks
    )
where

-- TODO: don't want a dependency of benchmark on test - move queries to a place inside benchmark.

import qualified Benchmarks.BayesianNetwork.Data                   as D
import           Criterion.Main
import qualified LocalComputation.Instances.BayesianNetwork        as BN
import qualified LocalComputation.Instances.BayesianNetwork.Parser as P
import qualified LocalComputation.LocalProcess                     as P
import qualified LocalComputation.Utils                            as U
import           Text.Pretty.Simple                                (pShowNoColor)

data WithName a = WithName {
      name  :: String
    , value :: a
}

benchmarks :: IO [Benchmark]
benchmarks = do
        -- TODO: Are we sure these are computed before the first benchmark starts?
        smallNet  <- fmap (WithName "SmallNet") $ U.parseFile' P.network D.asiaFilepath
        mediumNet <- fmap (WithName "MediumNet") $ U.parseFile' P.network D.alarmFilepath

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
                  bench "localcomputation-current"   $ nfIO $ P.run $ BN.getProbability    queries net.value
                , bench "localcomputation-alternate" $ nfIO $ P.run $ BN.getProbabilityAlt queries net.value
            ]

{-

>>> U.parseFile' P.network D.alarmFilepath >>= (\x -> D.sample $ D.genQueries x 10 2 3)
[
Query {conditioned = fromList [("INSUFFANESTH","FALSE")], conditional = fromList [("KINKEDTUBE","FALSE")]},
Query {conditioned = fromList [("LVFAILURE","FALSE")], conditional = fromList [("EXPCO2","ZERO"),("PAP","NORMAL"),("VENTLUNG","LOW")]},
Query {conditioned = fromList [("PCWP","NORMAL")], conditional = fromList []},
Query {conditioned = fromList [("ERRLOWOUTPUT","FALSE"),("TPR","NORMAL")], conditional = fromList [("CO","NORMAL")]},
Query {conditioned = fromList [("PAP","LOW")], conditional = fromList []},
Query {conditioned = fromList [("INSUFFANESTH","FALSE")], conditional = fromList []},
Query {conditioned = fromList [("INSUFFANESTH","TRUE")], conditional = fromList [("TPR","HIGH")]},
Query {conditioned = fromList [("HYPOVOLEMIA","TRUE")], conditional = fromList [("ARTCO2","HIGH"),("SHUNT","HIGH"),("VENTMACH","NORMAL")]},
Query {conditioned = fromList [("MINVOLSET","HIGH")], conditional = fromList []},
Query {conditioned = fromList [("ERRLOWOUTPUT","FALSE"),("TPR","LOW")], conditional = fromList [("INSUFFANESTH","TRUE"),("SHUNT","NORMAL")]}]

-}


-- bayesianNetwork = fmap fromRight $ parseFile P.network B.asiaFilepath
-- bayesianQuery = (M.fromList [("either", True)], M.fromList [("asia", True), ("tub", False), ("smoke", False), ("bronc", True)])
--
-- bayesian n = run $ do
--     result <- B.getProbability [bayesianQuery] n
--     -- pTraceShow result (pure result)
--     pure result
--
