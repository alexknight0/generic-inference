module Benchmarks.Utils (
      sample
    , getSeed
    , Seed
    , Word64
) where
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

------------------------------------------------------------------------

-- Sampling

-- | Generate a sample from a generator.
--
-- This function is useful for examining a 'Gen' in GHCi or other contexts.
-- It is not appropriate for use in a test suite directly. You will only
-- get a single sample from this function, and it will not give you
-- a property test. The seed is random, so the test is not deterministic.
--
-- If you only want a single test to run, then use @'withTests' 1@:
--
-- @
-- prop_OnlyRunOnce :: Property
-- prop_OnlyRunOnce =
--   'withTests' 1 $ 'property' $ do
--     i <- Gen.int
--     i /== 0
-- @
sample :: (HasCallStack, MonadIO m) => Seed -> Gen a -> m a
sample initialSeed gen =
  withFrozenCallStack $
    liftIO $
      let
        loop n seed =
          if n <= 0 then
            error "Hedgehog.Gen.sample: too many discards, could not generate a sample"
          else do
            let (thisSeed, nextSeed) = split seed
            case evalGen 30 thisSeed gen of
              Nothing ->
                loop (n - 1) nextSeed
              Just x ->
                pure $ Tree.treeValue x
      in
        loop (100 :: Int) initialSeed

getSeed :: Word64 -> Seed
getSeed = from
