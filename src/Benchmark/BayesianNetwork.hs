module Benchmark.BayesianNetwork
    (
        benchmarks
    )
where

-- TODO: don't want a dependency of benchmark on test - move queries to a place inside benchmark.

import           Criterion.Main

benchmarks :: IO Benchmark
benchmarks = do
        pure $ bgroup "Bayesian" [
                      bench "localcomputation" $ nfIO $ print "placeholder"
                    , bench "placeholder"      $ nfIO $ print "placeholder"
                ]


-- bayesianNetwork = fmap fromRight $ parseFile P.network B.asiaFilepath
-- bayesianQuery = (M.fromList [("either", True)], M.fromList [("asia", True), ("tub", False), ("smoke", False), ("bronc", True)])
--
-- bayesian n = run $ do
--     result <- B.getProbability [bayesianQuery] n
--     -- pTraceShow result (pure result)
--     pure result
--
