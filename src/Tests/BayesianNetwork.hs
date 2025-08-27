{-# LANGUAGE TemplateHaskell #-}

module Tests.BayesianNetwork
    ( tests, dataToValuations )
where

import           Benchmark.Baseline.Probability
import           LocalComputation.Instances.BayesianNetwork
import qualified LocalComputation.Instances.BayesianNetwork.Parser as P
import           LocalComputation.LocalProcess
import           LocalComputation.Utils
import           LocalComputation.ValuationAlgebra.Semiring
import           Tests.BayesianNetwork.Data

import           Hedgehog
import qualified Hedgehog.Gen                                      as Gen
import qualified Hedgehog.Range                                    as Range

import           Control.DeepSeq                                   (force)
import           Control.Distributed.Process
import           Control.Distributed.Process.Serializable          (Serializable)
import qualified Control.Exception                                 as E
import           Control.Monad                                     (forM)
import qualified Data.Hashable                                     as H
import qualified Data.Set                                          as S
import           Tests.Utils                                       (checkAnswers,
                                                                    unitTest)

import           Control.DeepSeq                                   (NFData)
import           LocalComputation.Inference                        (Mode (Shenoy),
                                                                    queriesDrawGraph)

tests :: IO Bool
tests = checkParallel $$(discover)

probabilityApproxEqual :: Probability -> Probability -> Bool
probabilityApproxEqual x y = abs (x - y) < 0.0002

dataToValuations :: [([AsiaVar], [Probability])] -> Network AsiaVar Bool
dataToValuations vs = map (uncurry getRows) withVariableDomains
    where
        withVariableDomains :: [([(AsiaVar, [Bool])], [Probability])]
        withVariableDomains = map (\(xs, ps) -> (map boolify xs, ps)) vs

checkQueries :: (H.Hashable a, H.Hashable b, Show a, Show b, Serializable a, Serializable b, Ord a, Ord b, NFData a, NFData b)
    => [ProbabilityQuery a b]
    -> [Probability]
    -> PropertyT IO (Network a b)
    -> PropertyT IO (Network a b)
checkQueries qs ps getNetwork = do
    network <- getNetwork
    results <- run $ getProbability qs network
    checkAnswers probabilityApproxEqual results ps
    pure network

parseNetwork'' :: FilePath -> PropertyT IO (Network String Bool)
parseNetwork'' filename = do
    parsed <- liftIO $ parseFile P.network filename
    case parsed of
        Left e        -> do annotateShow e; failure
        Right network -> pure (network)

boolify :: a -> (a, [Bool])
boolify x = (x, [False, True])

inferenceAnswers :: [ProbabilityQuery AsiaVar Bool] -> [Probability] -> [([AsiaVar], [Probability])] -> Property
inferenceAnswers qs as vs = unitTest $ checkQueries qs as (pure $ dataToValuations vs)

-- prop_drawGraph :: Property
-- prop_drawGraph = unitTest $ do
--     fromRight $ queriesDrawGraph "test.svg" Shenoy (dataToValuations asiaValuationsP1) asiaQueriesP1
--     pure ()

prop_inferenceAnswersP1 :: Property
prop_inferenceAnswersP1 = inferenceAnswers asiaQueriesP1 asiaAnswersP1 asiaValuationsP1

prop_inferenceAnswersP2 :: Property
prop_inferenceAnswersP2 = inferenceAnswers asiaQueriesP2 asiaAnswersP2 asiaValuationsP2

-- The valuations for this test differ from the valuations inside the asia.net file.
prop_inferenceAnswersP3 :: Property
prop_inferenceAnswersP3 = inferenceAnswers asiaQueriesP3 asiaAnswersP3 asiaValuationsP3

prop_inferenceAnswersAfterParsingP1 :: Property
prop_inferenceAnswersAfterParsingP1 = unitTest $ checkQueries asiaQueriesP1 asiaAnswersP1 $ do
    network <- parseNetwork'' asiaFilepath
    pure $ map (mapTableKeys stringToAsiaVar) network

prop_inferenceAnswersAfterParsingP2 :: Property
prop_inferenceAnswersAfterParsingP2 = unitTest $ checkQueries asiaQueriesP2 asiaAnswersP2 $ do
    network <- parseNetwork'' asiaFilepath
    pure $ map (mapTableKeys stringToAsiaVar) network

prop_prebuiltAnswersP1 :: Property
prop_prebuiltAnswersP1 = unitTest $ do
    let results = runQueries (createNetwork asiaValuationsP1) asiaQueriesP1
    checkAnswers probabilityApproxEqual results asiaAnswersP1

prop_prebuiltAnswersP3 :: Property
prop_prebuiltAnswersP3 = unitTest $ do
    let results = runQueries (createNetwork asiaValuationsP3) asiaQueriesP3
    checkAnswers probabilityApproxEqual results asiaAnswersP3

genQuery :: Gen (ProbabilityQuery AsiaVar Bool)
genQuery = do
    vars <- genVarsWithAssignedValue
    conditionedVarIndex <- Gen.int (Range.linear 0 (length vars - 1))
    let conditionedVar = vars !! conditionedVarIndex
        conditionalVars = filter (\x -> x /= conditionedVar) vars
    pure $ toProbabilityQuery ([conditionedVar], conditionalVars)

    where
        genVars = Gen.set (Range.linear 1 numVarsToChooseFrom)
                          (Gen.enum minAsiaP1 maxAsiaP1)

        genVarsWithAssignedValue = do
            vars <- genVars
            forM (S.toList vars) $ \x -> do
                b <- Gen.bool
                pure (x, b)

        numVarsToChooseFrom = length [minAsiaP1 .. maxAsiaP1]

prop_inferenceAnswersMatchPrebuilt :: Property
prop_inferenceAnswersMatchPrebuilt = withTests 100 . property $ do
    qs <- forAll genQueries
    prebuiltResults' <- prebuiltResults qs

    case prebuiltResults' of
        -- Some queries result in underflow as when we incrementally impose conditions as in
        -- the prebuit case, we may attempt to impose a condition that has a 0% chance of occuring.
        -- Discard these cases.
        Left (E.RatioZeroDenominator) -> discard
        Left _ -> failure
        Right prebuiltResults'' -> do
            algebraResults' <- algebraResults qs
            checkAnswers probabilityApproxEqual algebraResults' prebuiltResults''

    where
        genQueries :: Gen ([ProbabilityQuery AsiaVar Bool])
        genQueries = Gen.list (Range.linear 1 6) genQuery

        algebraResults qs = run $ getProbability qs (dataToValuations asiaValuationsP1)
        prebuiltResults qs = liftIO $ E.try $ E.evaluate $ force $ runQueries (createNetwork asiaValuationsP1) qs

prop_parsesAndes :: Property
prop_parsesAndes = unitTest $ parseNetwork'' andesFilepath

