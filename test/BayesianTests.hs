{-# LANGUAGE TemplateHaskell #-}

module BayesianTests
    ( tests )
where

import Data
import Bayesian
import LocalProcess

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Data.Functor (void)
import Control.Concurrent (threadDelay)
import Control.Distributed.Process
import Control.Distributed.Process.Node

{-
- Our tests need to run with 
-}

tests :: IO Bool
tests = checkSequential $$(discover)

prop_matchesKnownQueryAnswers :: Property
prop_matchesKnownQueryAnswers = withTests 200 . property $ do
    results <- liftIO $ runProcessLocal' (queryNetwork asiaDefaultQueries asiaDefaultValuations)

    sequence_ $ zipWith (\x y -> diff x probabilityApproxEqual y) results [0.05, 0.0595, 1, 0.01, 0.1855, 0.099, 0.686]
    --queryNetwork asiaDefaultQueries asiaDefaultValuations
    


probabilityApproxEqual :: Probability -> Probability -> Bool
probabilityApproxEqual x y = abs (x - y) < 0.0002
