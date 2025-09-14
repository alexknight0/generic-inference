{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

{- | Module containing both tests for the utilities in the local computation library,
as well as general utilities used by the test library.
-}
module Tests.Utils (
      tests
    , genMode
    , unitTest
    , checkAnswers
) where

import           Hedgehog
import qualified Hedgehog.Gen                              as Gen
import qualified Hedgehog.Range                            as Range

import           Control.DeepSeq                           (force)
import           Control.Distributed.Process
import qualified Control.Exception                         as E
import           Control.Monad                             (void)
import qualified LocalComputation.Inference                as I
import qualified LocalComputation.Inference.MessagePassing as MP
import           LocalComputation.Utils                    (zipWithAssert)

-------------------------------------------------------------------------------
-- Tests                                                                     --
-------------------------------------------------------------------------------
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

-------------------------------------------------------------------------------
-- Utils                                                                     --
-------------------------------------------------------------------------------

genMode :: Gen I.Mode
genMode = Gen.element [I.BruteForce, I.Fusion, (I.Shenoy MP.Threads)]

unitTest :: PropertyT IO a -> Property
unitTest = withTests 1 . property . void

checkAnswers :: (Show a, Show b) => (a -> b -> Bool) -> [a] -> [b] -> PropertyT IO ()
checkAnswers f results answers = diff results (\rs as -> and (zipWithAssert f rs as)) answers

