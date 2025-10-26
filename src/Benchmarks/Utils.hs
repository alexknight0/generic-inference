{-# LANGUAGE CPP #-}
-- | Utilities for the benchmarks suite.
--
-- This module depends on hedgehog internals and hence is
-- highly vulnerable to future updates to the hedgehog library.
module Benchmarks.Utils (
      sample
    , getSeed
    , Seed
    , Word64
    , benchmarkComplexity
) where

-- These imports match imports used in hedgehog internals
import           Control.Monad.IO.Class                (MonadIO (..))

import           Data.Word                             (Word64)

import           Hedgehog.Internal.Gen                 (Gen, evalGen)
import           Hedgehog.Internal.Seed                (Seed, from, split)
import           Hedgehog.Internal.Source              (HasCallStack,
                                                        withFrozenCallStack)
import qualified Hedgehog.Internal.Tree                as Tree
import           Hedgehog.Range                        (Size)

import           Control.DeepSeq                       (rnf)
import           Control.Exception                     (evaluate)
import qualified Data.List                             as L
import qualified LocalComputation.Inference.Statistics as S
import qualified LocalComputation.Utils                as U
import qualified LocalComputation.ValuationAlgebra     as V
import           System.IO                             (IOMode (AppendMode),
                                                        hFlush, hPutStrLn,
                                                        stdout, withFile)
import qualified System.Random                         as R

--------------------------------------------------------------------------------
-- Settings
--------------------------------------------------------------------------------
isCountingOperations :: Bool
#if !defined(COUNT_OPERATIONS) || !(COUNT_OPERATIONS)
isCountingOperations = False
#else
isCountingOperations = True
#endif

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------
-- | Generate a sample from a generator, with a set seed.
--
-- This function is useful for examining a 'Gen' in GHCi or other contexts.
-- It is not appropriate for use in a test suite directly. You will only
-- get a single sample from this function, and it will not give you
-- a property test.
sample :: (HasCallStack, MonadIO m) => Int -> Gen a -> m a
sample seed gen = withFrozenCallStack $ liftIO $ trySample (getSeed seedForGen)     -- Seed for generator
                                                           gen                      -- Generator
                                                           (fromInteger randomSize) -- Random value for size
                                                           (100 :: Int)             -- Num discards before failure

    where
        (randomSize, newGen) = R.randomR (0, 100) (R.mkStdGen seed)
        (seedForGen, _)      = R.randomR (0, maxBound :: Word64) newGen

trySample :: (Ord t, Num t, Applicative f) => Seed -> Gen a -> Size -> t -> f a
trySample seed gen size n
    | n <= 0 = error "Hedgehog.Gen.sample: too many discards, could not generate a sample"
    | otherwise = case evalGen size currentSeed gen of
                      Nothing -> trySample nextSeed gen size (n - 1)
                      Just x  -> pure $ Tree.treeValue x
    where
        (currentSeed, nextSeed) = split seed

getSeed :: Word64 -> Seed
getSeed = from


complexityFilepath :: FilePath
complexityFilepath = "operation_count.csv"

-- | Counts certain operations, allowing metrics for computational complexity to be estimated.
benchmarkComplexity :: (V.NFData a)
    => [String] -> IO (S.WithStats a) -> (S.Stats -> Int) -> IO ()
benchmarkComplexity header problem complexity = do

    if not isCountingOperations
        then error "Not counting operations."
        else pure ()

    U.resetGlobal V.combineCounter
    U.resetGlobal V.projectCounter

    putStrLn ("Working on: " ++ L.intercalate "/" header)
    hFlush stdout

    -- Fully evaluate, incrementing combine and project counters
    result <- problem
    evaluate (rnf result)

    -- Write combination count to file.
    withFile complexityFilepath AppendMode $ \h -> do
        combinations   <- U.getGlobal V.combineCounter
        projections    <- U.getGlobal V.projectCounter

        hPutStrLn h $ line [ show $ combinations
                           , show $ projections
                           , show $ maximum result.stats.treeWidths
                           , show $ maximum result.stats.valuations
                           , show $ complexity result.stats
                           , show $ result.stats.valuations
                           , show $ result.stats.treeVertices
                           , show $ result.stats.sumFrameLengths
                          ]

        hFlush h

    where
        line body = L.intercalate "," $ header ++ map show body
