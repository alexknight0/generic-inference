{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LocalComputation.Potential (

    Potential
  , toFrames
  , toValues
  , project
  , combine
  , unsafeCreate
  , unsafeCreate'
  , unsafeFromList
  , unsafeGetAssignment
  , unsafeGetValue
  , null
  , permutationMap
  , fromPermutationMap
  , mapFrames
  , mapVariables
  , unsafeGetIndex
  , frame

) where

import           Control.Exception                 (assert)
import qualified Data.Array                        as A
import qualified Data.Map                          as M
import           Data.Maybe                        (fromJust)
import qualified Data.Set                          as Set
import           GHC.Stack                         (HasCallStack)
import qualified LocalComputation.IndexedSet       as S
import qualified LocalComputation.Utils            as A (listArray0)
import qualified LocalComputation.Utils            as M (unionA, unsafeLookup)
import qualified LocalComputation.Utils            as U
import           LocalComputation.ValuationAlgebra (Binary, Generic, NFData)
import           Prelude                           hiding (null)

-- TODO: The 'frames' field of 'Potential' should probably be updated to use a list over
-- an 'IndexedSet' as in most cases the 'IndexedSet' will likely be so small that a normal
-- list lookup would be more efficient.

{- | Stores values in the following order:

    Assignments |    Value
       A   B    |
  --------------|--------------
       0   0    |     0.7
                |
       0   1    |     0.4
                |
       0   2    |     0.3
                |
       1   0    |     0.6
                |
       .   .    |      .
                |
       .   .    |      .
                |

Where assignments and the values assigned to them are ordered using the Ord instance.
-}
data Potential a b c = Potential { frames :: M.Map a (S.IndexedSet b), values :: A.Array Int c }
                                    deriving (Binary, Generic, NFData)

unsafeCreate :: HasCallStack => M.Map a (S.IndexedSet b) -> A.Array Int c -> Potential a b c
unsafeCreate frames values = U.assertP satisfiesInvariants $ Potential frames values

unsafeCreate' :: M.Map a (Set.Set b) -> A.Array Int c -> Potential a b c
unsafeCreate' frames values = unsafeCreate (M.map S.fromSet frames) values

unsafeFromList :: forall c b a. (Ord a, Ord b) => [(a, [b])] -> [c] -> Potential a b c
unsafeFromList frames values = unsafeFromList' $ zip (toPermutations frames) values

unsafeFromList' :: forall c b a. (Ord a, Ord b) => [([(a, b)], c)] -> Potential a b c
unsafeFromList' rows = fromPermutationMap (M.mapKeys M.fromList $ M.fromList rows)

{- | Satisfies invariants if:
    1. The number of permutations is equal to the number of values
-}
satisfiesInvariants :: Potential a b c -> Bool
satisfiesInvariants p = numPermutations p.frames == length p.values -- (1)

instance (Ord b) => Functor (Potential a b) where
    fmap f p = unsafeCreate p.frames $ fmap f p.values

combine :: (Ord a, Ord b) => (c -> c -> c) -> Potential a b c -> Potential a b c -> Potential a b c
combine union p1 p2 = unsafeCreate newFrame newValues
    where
        newValues = A.listArray0 $ map f (permutationList newFrame)

        f assignment = union (getValueP1 $ M.intersection assignment p1.frames)
                             (getValueP2 $ M.intersection assignment p2.frames)
            where
                getValueP1 = unsafeGetValue' p1 (M.toAscList p1.frames) (getFactors $ M.toAscList p1.frames)
                getValueP2 = unsafeGetValue' p2 (M.toAscList p2.frames) (getFactors $ M.toAscList p2.frames)

        newFrame = M.unionA p1.frames p2.frames

project :: forall a b c . (Ord a, Ord b) => (c -> c -> c) -> Potential a b c -> Set.Set a  -> Potential a b c
project union p domain = unsafeCreate newFrames newValues
    where

        newFrames = M.restrictKeys p.frames domain

        newValues = fmap fromJust $ A.accumArray g Nothing (0, numPermutations newFrames - 1) newIndicesOfValues
            where
                g acc x = case acc of
                            Nothing -> Just x
                            Just y  -> Just $ union x y

        newIndicesOfValues = zipWith f (permutationList p.frames) (A.elems p.values)
            where
                f permutation value = (getIndex (M.restrictKeys permutation domain), value)

                getIndex = unsafeGetIndex' (M.toAscList newFrames) (getFactors $ M.toAscList newFrames)

numPermutations :: M.Map a (S.IndexedSet b) -> Int
numPermutations = product . map S.size . M.elems

-- TODO: There exists a slightly more efficent way to do this.
unsafeGetAssignment :: M.Map a (S.IndexedSet b) -> Int -> M.Map a b
unsafeGetAssignment frames index = permutationList frames !! index

unsafeGetIndex :: (Ord a, Ord b) => M.Map a (S.IndexedSet b) -> M.Map a b -> Int
unsafeGetIndex frames = unsafeGetIndex' (M.toAscList frames) (getFactors $ M.toAscList frames)

-- | Variant of `unsafeGetIndex` that has `M.toAscList` applied to its second parameter and factors precalculated
unsafeGetIndex' :: (Eq a, Ord b) => [(a, S.IndexedSet b)] -> [Int] -> M.Map a b -> Int
unsafeGetIndex' frames factors assignment = sum $ zipWith (*) choiceIndices factors
    where
        choiceIndices = U.zipWithA f (M.toAscList assignment) frames

        f (var, value) (_var, values) = assert (var == _var) $ S.unsafeElemIndex values value

getFactors :: [(a, S.IndexedSet b)] -> [Int]
getFactors frames = tail $ scanr (*) 1 numChoices
    where
        numChoices = map (S.size . snd) frames

-- | Returns the value of a given variable assignment in a given potential
unsafeGetValue :: (Ord a, Ord b) => Potential a b c -> M.Map a b -> c
unsafeGetValue p = unsafeGetValue' p (M.toAscList p.frames) (getFactors $ M.toAscList p.frames)

-- | Variant of `unsafeGetValue` that has `M.toAscList` applied to `p.frame` as a parameter and factors precalculated
unsafeGetValue' :: (Ord a, Ord b) => Potential a b c -> [(a, S.IndexedSet b)] -> [Int] -> M.Map a b -> c
unsafeGetValue' p frames factors a = p.values A.! unsafeGetIndex' frames factors a

permutationList :: M.Map a (S.IndexedSet b) -> [M.Map a b]
permutationList m = map M.fromDistinctAscList $ toPermutations
                                              $ M.toAscList
                                              $ M.map S.toList
                                              $ m

permutationMap :: (Ord a, Ord b) => Potential a b c -> M.Map (M.Map a b) c
permutationMap p = M.fromList $ zip (permutationList p.frames) (A.elems p.values)

fromPermutationMap :: (Ord a, Ord b) => M.Map (M.Map a b) c -> Potential a b c
fromPermutationMap m = unsafeCreate frames (A.listArray0 $ M.elems m)
    where
        frames = M.map S.fromSet $ foldr f M.empty $ map (M.map Set.singleton) $ M.keys m

        f = M.unionWith (Set.union)


mapVariables :: forall a1 a2 b c . (Ord a1, Ord a2, Ord b) => (a1 -> a2) -> Potential a1 b c -> Potential a2 b c
mapVariables f p = unsafeCreate newFrames (A.listArray0 $ M.elems newPermutationMap)
    where
        newFrames = M.mapKeys f p.frames

        newPermutationMap :: M.Map (M.Map a2 b) c
        newPermutationMap = M.mapKeys (M.mapKeys f) $ permutationMap p

-- | Assumes the given function is injective.
mapFrames :: forall a b1 b2 c . (Ord a, Ord b1, Ord b2) => (b1 -> b2) -> Potential a b1 c -> Potential a b2 c
mapFrames f p = unsafeCreate newFrames (A.listArray0 $ M.elems newPermutationMap)
    where
        newFrames = M.map (S.unsafeMap f) p.frames

        newPermutationMap :: M.Map (M.Map a b2) c
        newPermutationMap = M.mapKeys (M.map f) $ permutationMap p


toFrames :: Potential a b c -> M.Map a (Set.Set b)
toFrames = M.map S.toSet . (.frames)

toValues :: Potential a b c -> [c]
toValues = A.elems . (.values)


null :: Potential a b c -> Bool
null p = M.null p.frames

frame :: (Ord a) => a -> Potential a b c -> Set.Set b
frame x p = S.toSet $ M.unsafeLookup x p.frames

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

toPermutations :: [(a, [b])] -> [[(a, b)]]
toPermutations [] = [[]]
toPermutations ((v, possibilites) : others) = [(v, value) : rest
                                               | value <- possibilites, rest <- toPermutations others]

