{-# LANGUAGE ScopedTypeVariables #-}
module LocalComputation.IndexableSet (
      IndexableSet
    , unsafeFromList
    , toList
    , unsafeLookup
    , unsafeElemIndex
    , size
) where
import qualified Data.List              as L
import           Data.Maybe             (fromJust)
import qualified Data.Set               as S
import qualified LocalComputation.Utils as U
import           Prelude                hiding (lookup)

newtype IndexableSet a = IndexableSet { l :: [a] } deriving Eq

unsafeFromList m = U.assertP satisfiesInvariants . IndexableSet

toList :: IndexableSet a -> [a]
toList s = s.l

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

