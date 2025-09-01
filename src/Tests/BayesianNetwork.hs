{-# LANGUAGE TemplateHaskell #-}

module Tests.BayesianNetwork
    ( tests, dataToValuations )
where

import           Benchmarks.BayesianNetwork.Baseline
import           Benchmarks.BayesianNetwork.Data
import           LocalComputation.Instances.BayesianNetwork
import qualified LocalComputation.Instances.BayesianNetwork.Parser as P
import           LocalComputation.LocalProcess
import           LocalComputation.Utils
import           LocalComputation.ValuationAlgebra.Semiring

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
import qualified Data.Char                                         as C
import           LocalComputation.Inference                        (Mode (Shenoy),
                                                                    queriesDrawGraph)

tests :: IO Bool
tests = checkParallel $$(discover)

-- This value is not as precise as might be expected as the values entere for manual testcases
-- are truncated to ~7 / 8 significant figures.
approxEqual :: Probability -> Probability -> Bool
approxEqual x y = abs (x - y) < 1 * 10 ^^ (-7 :: Integer)

dataToValuations :: [([AsiaVar], [Probability])] -> Network AsiaVar Bool
dataToValuations vs = map (uncurry getRows) withVariableDomains
    where
        withVariableDomains :: [([(AsiaVar, [Bool])], [Probability])]
        withVariableDomains = map (\(xs, ps) -> (map boolify xs, ps)) vs

checkQueries :: (Show a, Show b, Serializable a, Serializable b, Ord a, Ord b, NFData a, NFData b)
    => [Query a b]
    -> [Probability]
    -> PropertyT IO (Network a b)
    -> PropertyT IO (Network a b)
checkQueries qs ps getNetwork = do
    network <- getNetwork
    results <- run $ getProbability qs network
    checkAnswers approxEqual results ps
    pure network

parseNetwork :: FilePath -> PropertyT IO (Network String String)
parseNetwork filename = do
    parsed <- liftIO $ parseFile P.network filename
    case parsed of
        Left e        -> do annotateShow e; failure
        Right network -> pure network


convertToAsia :: Network String String -> Network AsiaVar Bool
convertToAsia network = map (mapTableKeys stringToAsiaVar) . boolifyNet $ network

boolifyNet :: Network String String -> Network String Bool
boolifyNet = map (mapVariableValues readBool)
    where
        readBool xs = case lowercase xs of
            "true"  -> True
            "false" -> False
            "yes"   -> True
            "no"    -> False
            _       -> error $ "Couldn't parse \"" ++ xs ++ "\" as a Bool."

        lowercase xs = map C.toLower xs

boolify :: a -> (a, [Bool])
boolify x = (x, [False, True])

inferenceAnswers :: [Query AsiaVar Bool] -> [Probability] -> [([AsiaVar], [Probability])] -> Property
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
prop_inferenceAnswersAfterParsingP1 = unitTest $ checkQueries asiaQueriesP1
                                                              asiaAnswersP1
                                                              (fmap convertToAsia $ parseNetwork asiaFilepath)

prop_inferenceAnswersAfterParsingP2 :: Property
prop_inferenceAnswersAfterParsingP2 = unitTest $ checkQueries asiaQueriesP2
                                                              asiaAnswersP2
                                                              (fmap convertToAsia $ parseNetwork asiaFilepath)

prop_prebuiltAnswersP1 :: Property
prop_prebuiltAnswersP1 = unitTest $ do
    let results = runQueries (createNetwork asiaValuationsP1) asiaQueriesP1
    checkAnswers approxEqual results asiaAnswersP1

prop_prebuiltAnswersP3 :: Property
prop_prebuiltAnswersP3 = unitTest $ do
    let results = runQueries (createNetwork asiaValuationsP3) asiaQueriesP3
    checkAnswers approxEqual results asiaAnswersP3

genQuery :: Gen (Query AsiaVar Bool)
genQuery = do
    vars <- genVarsWithAssignedValue
    conditionedVarIndex <- Gen.int (Range.linear 0 (length vars - 1))
    let conditionedVar = vars !! conditionedVarIndex
        conditionalVars = filter (\x -> x /= conditionedVar) vars
    pure $ toQuery ([conditionedVar], conditionalVars)

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
    net <- fmap convertToAsia $ parseNetwork asiaFilepath

    case prebuiltResults' of
        -- Some queries result in underflow as when we incrementally impose conditions as in
        -- the prebuit case, we may attempt to impose a condition that has a 0% chance of occuring.
        -- Discard these cases.
        Left (E.RatioZeroDenominator) -> discard
        Left _ -> failure
        Right prebuiltResults'' -> do
            algebraResults' <- algebraResults qs net
            checkAnswers approxEqual algebraResults' prebuiltResults''

    where
        genQueries :: Gen ([Query AsiaVar Bool])
        genQueries = Gen.list (Range.linear 1 6) genQuery

        algebraResults qs net = run $ getProbability qs net
        prebuiltResults qs = liftIO $ E.try $ E.evaluate $ force $ runQueries (createNetwork asiaValuationsP1) qs

prop_drawAlarmGraph :: Property
prop_drawAlarmGraph = unitTest $ do
    net <- fmap convertToAsia $ parseNetwork asiaFilepath
    fromRight $ queriesDrawGraph "asia.svg" Shenoy net (toInferenceQuery asiaQueriesP1)
    pure ()

prop_parsesAlarm :: Property
prop_parsesAlarm = unitTest $ do
    checkQueries alarmQueries alarmAnswers (parseNetwork alarmFilepath)

-- prop_parsesMunin :: Property
-- prop_parsesMunin = unitTest $ do
--     checkQueries muninQueries muninAnswers (parseNetwork muninFilepath)


