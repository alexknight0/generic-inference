{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LocalComputation.IndexableSet (
      IndexableSet
    , unsafeFromList
    , toList
    , toSet
    , fromSet
    , unsafeLookup
    , unsafeElemIndex
    , size
    , map
) where
import qualified Data.List                         as L
import           Data.Maybe                        (fromJust)
import qualified Data.Set                          as S
import           GHC.Stack                         (HasCallStack)
import qualified LocalComputation.Utils            as U
import           LocalComputation.ValuationAlgebra (Binary, Generic, NFData)
import           Prelude                           hiding (lookup, map)

newtype IndexableSet a = IndexableSet { l :: [a] } deriving (Eq, Binary, Generic, NFData)

unsafeFromList :: (HasCallStack, Ord a) => [a] -> IndexableSet a
unsafeFromList = U.assertP satisfiesInvariants . IndexableSet

toList :: IndexableSet a -> [a]
toList s = s.l

toSet :: (Eq a) => IndexableSet a -> S.Set a
toSet s = S.fromAscList s.l

fromSet :: (Ord a) => S.Set a -> IndexableSet a
fromSet = unsafeFromList . S.toAscList

{- | Satisfies invariants if:
    1. The possible values for each variable are unique
    2. The possible values for each variable are ordered
-}
satisfiesInvariants :: (Ord a) => IndexableSet a -> Bool
satisfiesInvariants s = s.l == (S.toAscList $ S.fromList s.l)

unsafeLookup :: IndexableSet a -> Int -> a
unsafeLookup s i = s.l !! i

unsafeElemIndex :: (Eq a) => IndexableSet a -> a -> Int
unsafeElemIndex s x = fromJust $ L.elemIndex x s.l

size :: IndexableSet a -> Int
size s = length s.l

map :: (Ord b) => (a -> b) -> IndexableSet a -> IndexableSet b
map f s = IndexableSet $ S.toAscList
                       $ S.fromList
                       $ fmap f s.l


