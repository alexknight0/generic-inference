{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LocalComputation.IndexableSet (
      IndexableSet
    , unsafeFromList
    , toList
    , toSet
    , fromSet
    , unsafeElemIndex
    , size
    , unsafeMap
) where
import qualified Data.List                         as L
import qualified Data.Map.Strict                   as M
import           Data.Maybe                        (fromJust)
import qualified Data.Set                          as S
import           GHC.Stack                         (HasCallStack)
import qualified LocalComputation.Utils            as U
import           LocalComputation.ValuationAlgebra (Binary, Generic, NFData)
import           Prelude                           hiding (lookup, map)

newtype IndexableSet a = IndexableSet { m :: M.Map a Int } deriving (Eq, Binary, Generic, NFData)

unsafeFromMap :: M.Map a Int -> IndexableSet a
unsafeFromMap = U.assertP satisfiesInvariants . IndexableSet

unsafeFromList :: (HasCallStack, Ord a) => [a] -> IndexableSet a
unsafeFromList xs = unsafeFromMap . M.fromList $ zip xs [0..]

toList :: IndexableSet a -> [a]
toList s = M.keys s.m

toSet :: IndexableSet a -> S.Set a
toSet s = M.keysSet s.m

fromSet :: S.Set a -> IndexableSet a
fromSet s = unsafeFromMap . M.fromDistinctAscList $ zip (S.toAscList s) [0..]

{- | Satisfies invariants if:
    1. If we read the values in key order, it forms an enumeration.
-}
satisfiesInvariants :: IndexableSet a -> Bool
satisfiesInvariants s = M.elems s.m == [0 .. M.size s.m - 1]

unsafeElemIndex :: (Ord a) => IndexableSet a -> a -> Int
unsafeElemIndex s x = s.m M.! x

size :: IndexableSet a -> Int
size s = M.size s.m

unsafeMap :: (Ord b) => (a -> b) -> IndexableSet a -> IndexableSet b
unsafeMap f s = unsafeFromMap $ M.mapKeys f s.m

