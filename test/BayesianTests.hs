{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module BayesianTests
    ( tests )
where

import SemiringValuationAlgebra
import           Bayesian
import           Bayesian.HackageVersion
import           Bayesian.Parser
import           Data
import           LocalProcess

import           Hedgehog
import qualified Hedgehog.Gen                             as Gen
import qualified Hedgehog.Range                           as Range

import           Control.Concurrent                       (threadDelay)
import           Control.Distributed.Process
import           Control.Distributed.Process.Node
import           Control.Distributed.Process.Serializable (Serializable)
import           Data.Functor                             (void)

{-
- Our tests need to run with
-}



tests :: IO Bool
tests = checkParallel $$(discover)

probabilityApproxEqual :: Probability -> Probability -> Bool
probabilityApproxEqual x y = abs (x - y) < 0.0002

checkAnswers :: (Show a) => (a -> a -> Bool) -> [a] -> [a] -> PropertyT IO ()
checkAnswers f results answers = diff results (\rs as -> and (zipWith f rs as)) answers

checkQueries :: (Serializable a, Serializable b, Ord a, Ord b) => [ProbabilityQuery a b] -> [Probability] -> PropertyT IO (Network a b) -> PropertyT IO (Network a b)
checkQueries qs ps getNetwork = do
    network <- getNetwork
    results <- liftIO $ runProcessLocal $ queryNetwork qs network
    checkAnswers probabilityApproxEqual results ps
    pure network

parseNetwork'' :: FilePath -> PropertyT IO (Network String Bool)
parseNetwork'' filename = do
    parsed <- liftIO $ parseNetwork filename
    case parsed of
        Left e        -> do annotateShow e; failure
        Right network -> pure (network)

unitTest :: PropertyT IO a -> Property
unitTest = withTests 1 . property . void

prop_inferenceAnswersP1 :: Property
prop_inferenceAnswersP1 = unitTest $ checkQueries asiaQueriesP1 asiaAnswersP1 (pure $ map getRows asiaValuationsP1)

prop_inferenceAnswersP2 :: Property
prop_inferenceAnswersP2 = unitTest $ checkQueries asiaQueriesP2 asiaAnswersP2 (pure $ map getRows asiaValuationsP2)

-- The valuations for this test differ from the valuations inside the asia.net file.
prop_inferenceAnswersP3 :: Property
prop_inferenceAnswersP3 = unitTest $ checkQueries asiaQueriesP3 asiaAnswersP3 (pure $ map getRows asiaValuationsP3)

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

prop_parsesAndes :: Property
prop_parsesAndes = unitTest $ parseNetwork'' andesFilepath

