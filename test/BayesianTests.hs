{-# LANGUAGE TemplateHaskell #-}

module BayesianTests
    ( tests )
where

import           Bayesian
import           Data
import           LocalProcess

import           Hedgehog
import qualified Hedgehog.Gen                     as Gen
import qualified Hedgehog.Range                   as Range

import           Control.Concurrent               (threadDelay)
import           Control.Distributed.Process
import           Control.Distributed.Process.Node
import           Data.Functor                     (void)

{-
- Our tests need to run with
-}



tests :: IO Bool
tests = checkParallel $$(discover)

prop_matchesKnownQueryAnswers :: Property
prop_matchesKnownQueryAnswers = withTests 1 . property $ do
    results <- liftIO $ runProcessLocal (queryNetwork asiaDefaultQueries asiaDefaultValuations)

    sequence_ $ zipWith (\x y -> diff x probabilityApproxEqual y) results asiaDefaultAnswers

prop_parses_asia :: Property
prop_parses_asia = withTests 1 . property $ do
    parsed <- liftIO $ parseNetwork "data/asia.net"
    case parsed of
         Left e -> do annotateShow e; failure
         Right network -> do
             let mappedNetwork :: [BayesValuation AsiaVar Bool]
                 mappedNetwork = map (mapTableKeys stringToAsiaVar) network
             results <- liftIO $ runProcessLocal (queryNetwork asiaDefaultQueries mappedNetwork)
             sequence_ $ zipWith (\x y -> diff x probabilityApproxEqual y) results asiaDefaultAnswers




prop_parses_andes :: Property
prop_parses_andes = withTests 1 . property $ do
    parsed <- liftIO $ parseNetwork "data/andes.net"
    case parsed of
         Left e        -> do annotateShow e; failure
         Right network -> success


probabilityApproxEqual :: Probability -> Probability -> Bool
probabilityApproxEqual x y = abs (x - y) < 0.0002
