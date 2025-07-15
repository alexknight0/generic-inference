{-# LANGUAGE TemplateHaskell     #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tests.Utils
    ( tests )
where

import           Hedgehog
import qualified Hedgehog.Gen                             as Gen
import qualified Hedgehog.Range                           as Range

import           Control.Concurrent                       (threadDelay)
import           Control.DeepSeq                          (force)
import           Control.Distributed.Process
import           Control.Distributed.Process.Node
import           Control.Distributed.Process.Serializable (Serializable)
import qualified Control.Exception                        as E
import           Data.Functor                             (void)
import           Data.Maybe                               (isNothing)
import           Debug.Trace                              (traceShow,
                                                           traceShowId)
import           GHC.Float                                (properFractionDouble)
import           System.IO.Silently                       (capture)
import           Utils

import qualified Data.Matrix                              as M
import qualified LabelledMatrix                           as L

tests :: IO Bool
tests = checkParallel $$(discover)

prop_assertsAreStillPresent :: Property
prop_assertsAreStillPresent = withTests 100 . property $ do
    footnote "If this test fails, then optimizations are removing some useful \
              \assertions that should probably be active during testing. \
              \Try using `stack test --fast` (disables all optimizations)."
    x <- forAll $ Gen.int (Range.linear 1 100)
    result <- liftIO $ E.try $ E.evaluate $ force (f x == 4)
    case result of
        Left (E.AssertionFailed _) -> success
        Right _                    -> failure

    where
        -- If optimizations are enabled such that this assert is not triggered,
        -- then other asserts in the main code will also have been removed.
        f :: Int -> Int
        f _ | E.assert False False = undefined
        f _ = 4
