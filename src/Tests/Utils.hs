{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Tests.Utils
    ( tests )
where

import           Hedgehog
import qualified Hedgehog.Gen                as Gen
import qualified Hedgehog.Range              as Range

import           Control.DeepSeq             (force)
import           Control.Distributed.Process
import qualified Control.Exception           as E


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
