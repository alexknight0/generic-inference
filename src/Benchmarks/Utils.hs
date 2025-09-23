{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Utilities for the benchmarks suite.
--
-- This module depends on hedgehog internals and hence is
-- highly vulnerable to future updates to the hedgehog library.
module Benchmarks.Utils (
      sample
    , getSeed
    , Seed
    , Word64
) where

-- These imports match imports used in hedgehog internals
import           Control.Applicative            (Alternative (..), liftA2)
import           Control.Monad                  (MonadPlus (..), filterM, guard,
                                                 join, replicateM)
import           Control.Monad.IO.Class         (MonadIO (..))
import           Control.Monad.Zip              (MonadZip (..))

import           Data.Bifunctor                 (first)
import           Data.ByteString                (ByteString)
import qualified Data.ByteString                as ByteString
import qualified Data.Char                      as Char
import           Data.Foldable                  (for_, toList)
import           Data.Functor.Identity          (Identity (..))
import           Data.Int                       (Int16, Int32, Int64, Int8)
import           Data.Kind                      (Type)
import           Data.List.NonEmpty             (NonEmpty)
import qualified Data.List.NonEmpty             as NonEmpty
import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import qualified Data.Maybe                     as Maybe
import qualified Data.Semigroup                 as Semigroup
import           Data.Sequence                  (Seq)
import qualified Data.Sequence                  as Seq
import           Data.Set                       (Set)
import qualified Data.Set                       as Set
import           Data.Text                      (Text)
import qualified Data.Text                      as Text
import qualified Data.Text.Encoding             as Text
import           Data.Word                      (Word16, Word32, Word64, Word8)

import           Hedgehog.Internal.Distributive (MonadTransDistributive (..))
import           Hedgehog.Internal.Gen          (Gen, evalGen)
import           Hedgehog.Internal.Prelude      hiding (either, maybe, seq)
import           Hedgehog.Internal.Seed         (Seed, from, split)
import qualified Hedgehog.Internal.Seed         as Seed
import qualified Hedgehog.Internal.Shrink       as Shrink
import           Hedgehog.Internal.Source       (HasCallStack,
                                                 withFrozenCallStack)
import           Hedgehog.Internal.Tree         (NodeT (..), Tree, TreeT (..))
import qualified Hedgehog.Internal.Tree         as Tree
import           Hedgehog.Range                 (Range, Size)
import qualified Hedgehog.Range                 as Range

import qualified System.Random                  as R

------------------------------------------------------------------------

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
