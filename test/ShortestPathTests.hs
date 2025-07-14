{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module ShortestPathTests
    ( tests )
where

import           Data
import           LocalProcess
import qualified Instances.ShortestPath.SingleTarget                as ST
import           Utils
import           ValuationAlgebra.QuasiRegular

import           Hedgehog
import qualified Hedgehog.Gen                             as Gen
import qualified Hedgehog.Range                           as Range

import           Control.Concurrent                       (threadDelay)
import           Control.Distributed.Process              (Process, liftIO)
import           Control.Distributed.Process.Serializable (Serializable)
import           Control.Monad                            (forM, forM_)
import           Data.Functor                             (void)
import           System.IO.Silently                       (capture)

tests :: IO Bool
tests = checkSequential $$(discover)

tolerableError :: TropicalSemiringValue
tolerableError = 0.00000001

approx :: TropicalSemiringValue -> TropicalSemiringValue -> Bool
approx x y = abs (x - y) < tolerableError

prop_p1 :: Property
prop_p1 = withTests 1 . property $ do
    results <- liftIO $ runProcessLocal $ ST.answerQueries graphP1 (fst graphQueriesP1) (snd graphQueriesP1)
    checkAnswers approx results graphAnswersP1



