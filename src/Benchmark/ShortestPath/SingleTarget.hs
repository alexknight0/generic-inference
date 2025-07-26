module Benchmark.ShortestPath.SingleTarget
    (
        benchmarks
    )
where

import qualified Benchmark.Baseline.DjikstraSimple                    as H
import           Criterion.Main
import qualified LocalComputation.Instances.ShortestPath.SingleTarget as ST
import           LocalComputation.LocalProcess                        (runProcessLocal)
import           LocalComputation.ValuationAlgebra.QuasiRegular       (TropicalSemiringValue (T))

-- TODO: don't want a dependency of benchmark on test.
import           LocalComputation.Graph                               (nonSymmetricEdges)
import qualified Tests.ShortestPath.SingleTarget.Data                 as D
import           Text.Pretty.Simple                                   (pPrint)

benchmarks :: IO ()
benchmarks = do
    p3Small <- D.p3SmallGraph'
    pPrint $ nonSymmetricEdges p3Small
    results <- runProcessLocal $ ST.singleTarget [fmap T p3Small] [68] 69
    pPrint results

    -- defaultMain [
    --           bgroup "smallGraph" [
    --                   bench "localcomputation" $ nfIO (runProcessLocal $ ST.singleTarget [D.p1Graph] D.p1Queries.sources D.p1Queries.target)
    --                 , bench "djikstra"         $ nf (H.singleTarget D.p1Graph D.p1Queries.sources D.p1Queries.target) (T $ read "Infinity")
    --             ]
    --         , bgroup "mediumGraph" [
    --                   bench "localcomputation" $ nfIO (runProcessLocal $ ST.singleTarget [fmap T p3Small] [68] 69)
    --                 , bench "djikstra"         $ nf (H.singleTarget p3Small [68] 69) (read "Infinity")
    --             ]

    --     ]





