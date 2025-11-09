{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Tests.BayesianNetwork
    ( tests, dataToValuations )
where

import           Benchmarks.BayesianNetwork.Baseline
import           Benchmarks.BayesianNetwork.Data
import qualified LocalComputation.Inference.Statistics             as S
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
import           Tests.Utils                                       (checkAnswers,
                                                                    unitTest)

import           Control.DeepSeq                                   (NFData)
import qualified Data.Char                                         as C
import           LocalComputation.Inference                        (Mode (..),
                                                                    queriesWithStats)

import qualified Benchmarks.BayesianNetwork.Data                   as B
import qualified Benchmarks.Utils                                  as U
import qualified LocalComputation.Inference                        as I
import qualified LocalComputation.Inference.JoinTree.Diagram       as D
import qualified LocalComputation.Inference.MessagePassing         as MP

tests :: IO Bool
tests = checkParallel $$(discover)

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------
prop_genericP1 :: Property
prop_genericP1 = testGeneric asiaQueriesP1 asiaAnswersP1 asiaValuationsP1

prop_genericP2 :: Property
prop_genericP2 = testGeneric asiaQueriesP2 asiaAnswersP2 asiaValuationsP2

-- The valuations for this test differ from the valuations inside the asia.net file.
prop_genericP3 :: Property
prop_genericP3 = testGeneric asiaQueriesP3 asiaAnswersP3 asiaValuationsP3

prop_genericAfterParsingP1 :: Property
prop_genericAfterParsingP1 = unitTest $ checkQueries asiaQueriesP1
                                                              asiaAnswersP1
                                                              (fmap convertToAsia $ parseNetwork asiaFilepath)

prop_genericAfterParsingP2 :: Property
prop_genericAfterParsingP2 = unitTest $ checkQueries asiaQueriesP2
                                                              asiaAnswersP2
                                                              (fmap convertToAsia $ parseNetwork asiaFilepath)

prop_baselineP1 :: Property
prop_baselineP1 = unitTest $ do
    let results = runQueries (createNetwork asiaValuationsP1) asiaQueriesP1
    checkAnswers approxEqual results asiaAnswersP1

prop_baselineP3 :: Property
prop_baselineP3 = unitTest $ do
    let results = runQueries (createNetwork asiaValuationsP3) asiaQueriesP3
    checkAnswers approxEqual results asiaAnswersP3

prop_genericMatchesBaseline :: Property
prop_genericMatchesBaseline = withTests 100 . property $ do
    qs <- forAll genQueries'
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
            checkAnswers approxEqual algebraResults'.c prebuiltResults''

    where
        genQueries' :: Gen ([Query AsiaVar Bool])
        genQueries' = Gen.list (Range.linear 1 6)
                               (genAsiaQuery 1 4)  -- Baseline can only handle max 1 conditioned variable.

        algebraResults qs net = run $ getProbability (I.Shenoy MP.Distributed) D.def qs net
        prebuiltResults qs = liftIO $ E.try $ E.evaluate $ force $ runQueries (createNetwork asiaValuationsP1) qs

prop_alarm :: Property
prop_alarm = unitTest $ do
    checkQueries alarmQueries alarmAnswers (parseNetwork alarmFilepath)

prop_drawThesisExample :: Property
prop_drawThesisExample = unitTest $ do
    fromRight $ queriesWithStats settings (Shenoy MP.Threads) (dataToValuations thesisExampleValuations) (toInferenceQuery [thesisExampleQuery])

    where settings = D.def { D.beforeInference = Just "diagrams/thesis_before.svg"
                           , D.afterInference = Nothing
                          }

--------------------------------------------------------------------------------
-- Settings
--------------------------------------------------------------------------------
-- This value is not as precise as might be expected as the values entered for manual testcases
-- are truncated to ~7 / 8 significant figures.
approxEqual :: Probability -> Probability -> Bool
approxEqual x y = abs (x - y) < 1 * 10 ^^ (-7 :: Integer)

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------
testGeneric :: [Query AsiaVar Bool] -> [Probability] -> [([AsiaVar], [Probability])] -> Property
testGeneric qs as vs = unitTest $ checkQueries qs as (pure $ dataToValuations vs)

checkQueries :: (Show a, Show b, Serializable a, Serializable b, Ord a, Ord b, NFData a, NFData b)
    => [Query a b]
    -> [Probability]
    -> PropertyT IO (Network a b)
    -> PropertyT IO (Network a b)
checkQueries qs ps getNetwork = do
    network <- getNetwork
    results <- run $ getProbability (I.Shenoy MP.Distributed) D.def qs network
    checkAnswers approxEqual results.c ps
    pure network

dataToValuations :: forall a . (Ord a) => [([a], [Probability])] -> Network a Bool
dataToValuations vs = map (uncurry getRows) withVariableDomains
    where
        withVariableDomains :: [([(a, [Bool])], [Probability])]
        withVariableDomains = map (\(xs, ps) -> (map boolify xs, ps)) vs

parseNetwork :: FilePath -> PropertyT IO (Network String String)
parseNetwork filename = do
    parsed <- liftIO $ U.parseFile P.network filename
    case parsed of
        Left e        -> do annotateShow e; failure
        Right network -> pure network

--------------------------------------------------------------------------------
-- Utilities - converting asia problems to typed representation
--------------------------------------------------------------------------------
genAsiaQuery :: Int -> Int -> Gen (Query AsiaVar Bool)
genAsiaQuery maxConditioned maxConditional = B.genQuery maxConditioned maxConditional asiaVariables

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

