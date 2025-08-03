{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- | Tests the valuation algebra solving shortest path problems.
-- Also doubles up to test that all methods of inference produce
-- the same results - all tests verify this property.
module Tests.ShortestPath.SingleTarget
    ( tests )
where

import qualified Benchmark.Baseline.DjikstraSimple                            as H
import           Benchmark.Data.ShortestPath
import qualified LocalComputation.Graph                                       as G
import qualified LocalComputation.Instances.ShortestPath.SingleTarget         as ST
import           LocalComputation.Utils
import           LocalComputation.ValuationAlgebra.QuasiRegular

import           Hedgehog
import qualified Hedgehog.Gen                                                 as Gen
import qualified Hedgehog.Internal.Property                                   as Hedgehog (PropertyName (..))
import qualified Hedgehog.Range                                               as Range

import           Control.Distributed.Process                                  (Process,
                                                                               liftIO)
import           Control.Monad                                                (forM,
                                                                               forM_,
                                                                               zipWithM,
                                                                               zipWithM_)
import qualified Data.Set                                                     as S
import qualified LocalComputation.Instances.ShortestPath.Parser               as P
import qualified Text.Parsec                                                  as P

-- Typeclasses
import           Control.DeepSeq                                              (NFData)
import           Control.Monad.IO.Class                                       (MonadIO)
import           Data.Binary                                                  (Binary)
import qualified Data.Hashable                                                as H
import qualified LocalComputation.Inference                                   as I
import           LocalComputation.ValuationAlgebra.QuasiRegular.SemiringValue (toDouble)
import           Numeric.Natural                                              (Natural)
import           Tests.Utils                                                  (checkAnswers,
                                                                               unitTest)
import           Type.Reflection                                              (Typeable)

tests :: IO Bool
tests = fmap and $ sequence [
        checkParallel $$(discover)
      , p3MatchesBaseline
   ]

p3MatchesBaseline :: IO Bool
p3MatchesBaseline = do
    p3VerySmall <- P.fromValid p3VerySmallGraph
    checkParallel $ Group "Tests.ShortestPath.SingleTarget" $ map (getTest p3VerySmall) [I.BruteForce, I.Fusion, I.Shenoy]

    where
        getTest :: G.Graph Natural Double -> I.Mode -> (PropertyName, Property)
        getTest g mode = (Hedgehog.PropertyName $ "prop_p3VerySmall_MatchesBaseline_" ++ show mode, matchesBaseline mode g 100)


tolerableError :: Double
tolerableError = 0.00000001

approx :: Double -> Double -> Bool
approx x y
    | abs (x - y) < tolerableError = True
    | x == (read "Infinity") && y == (read "Infinity") = True
    | x == (read "-Infinity") && y == (read "-Infinity") = True
    | otherwise = False

-- | Choice between either a `Baseline` inference implementation taken from hackage,
-- or a inference implementation from the `Local` computation library.
data Implementation = Baseline | Local I.Mode

singleTarget :: (MonadTest m, NFData a,  MonadIO m, Show a, Binary a, Typeable a,  H.Hashable a, Ord a) => Implementation -> [G.Graph a Double] -> [a] -> a -> m [Double]
singleTarget Baseline      graphs sources target = pure $ H.singleTarget graphs sources target infinity
singleTarget (Local mode)  graphs sources target
    | Left _ <- result = failure
    | Right r <- result = r
    where
        result = ST.singleTarget mode graphs sources target

-- | Tests that graphs that are missing zero cost self loops throw an error.
-- For the reason behind this behaviour, see the documentation of `ST.singleTarget`
prop_p0 :: Property
prop_p0 = unitTest $ do
    forM_ p0Graphs $ \g -> do
        case results g of
            Left ST.MissingZeroCostSelfLoops -> success
            _                                -> failure
    where
        results :: G.Graph Integer Double -> Either ST.Error (Process [Double])
        results graph = ST.singleTarget I.Shenoy [graph] p0Queries.sources p0Queries.target

pX :: Problem -> Property
pX p = unitTest $ do
    forM [Baseline, Local I.BruteForce, Local I.Fusion, Local I.Shenoy] $ \mode -> do
        results <- singleTarget mode p.graphs p.q.sources p.q.target
        checkAnswers approx (results) p.answers

-- | Tests that the all implementations work for a set problem where one graph is given.
prop_p1 :: Property
prop_p1 = pX p1

-- | Tests that the all implementations work for a set problem where multiple graphs are given.
prop_p2 :: Property
prop_p2 = pX p2

-- | Checks the output of the local computation algorithms and the baseline algorithm match for a set of random queries.
matchesBaseline :: forall a . (NFData a, H.Hashable a, Binary a, Typeable a, Show a, Ord a)
    => I.Mode
    -> G.Graph a Double
    -> TestLimit
    -> Property
matchesBaseline mode g numTests = withTests numTests . property $ do
    query <- forAll $ genConnectedQuery reverseAdjacencyList

    baseline  <- go Baseline query
    local     <- go (Local mode) query

    checkAnswers approx local baseline

    where
        go :: (MonadTest m, MonadIO m) => Implementation -> ST.Query a -> m [Double]
        go m query = singleTarget m [g] query.sources query.target

        reverseAdjacencyList = G.reverseAdjacencyList g

-- | Parses the given graph. Fails if a parse error occurs.
parseGraph :: IO (Either P.ParseError (Either P.InvalidGraphFile a)) -> PropertyT IO a
parseGraph g = do
    parsed <- liftIO g
    case parsed of
        Left e  -> do annotateShow e; failure
        Right parseResult -> case parseResult of
            Left e  -> do annotateShow e; failure
            Right x -> pure x

-- | Tests the parser doesn't fail on a known working example.
prop_parser :: Property
prop_parser = unitTest $ do
    _ <- parseGraph p3SmallGraph
    parseGraph p3MediumGraph
