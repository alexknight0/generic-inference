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

) where

import           Control.Exception                 (assert)
import           Data.Binary                       (get, put)
import qualified Data.IntMap                       as IntMap
import qualified Data.List                         as L
import qualified Data.Map                          as M
import           Data.Maybe                        (fromJust)
import qualified Data.Set                          as Set
import qualified Data.Vector                       as V
import           GHC.Stack                         (HasCallStack)
import qualified LocalComputation.IndexedSet       as S
import qualified LocalComputation.Utils            as A (listArray0)
import qualified LocalComputation.Utils            as M (unionA)
import qualified LocalComputation.Utils            as U
import           LocalComputation.ValuationAlgebra (Binary, Generic, NFData)
import           Prelude                           hiding (null)


{- | Stores values in the following order:

    Assignments |    Value
       A   B    |
  --------------|--------------
       0   0    |     9
       0   1    |     2
       0   2    |     8
       1   0    |     6
       1   1    |     5
       1   2    |     1

Where assignments and the values assigned to them are ordered using the Ord instance.
-}
data Potential a b c = Potential { frames :: M.Map a (S.IndexedSet b), values :: V.Vector c }
                                    deriving (Generic, NFData)

instance (Binary a, Binary b, Binary c) => Binary (Potential a b c) where
    put p = do
        put p.frames
        put (V.toList p.values)

    get = do
        frames <- get
        values <- get
        pure $ Potential frames (V.fromList values)


unsafeCreate :: HasCallStack => M.Map a (S.IndexedSet b) -> V.Vector c -> Potential a b c
unsafeCreate frames values = U.assertP satisfiesInvariants $ Potential frames values

unsafeCreate' :: M.Map a (Set.Set b) -> V.Vector c -> Potential a b c
unsafeCreate' frames values = unsafeCreate (M.map S.fromSet frames) values

unsafeFromList :: forall c b a. (HasCallStack, Ord a, Ord b) => [(a, [b])] -> [c] -> Potential a b c
unsafeFromList frames values = unsafeFromList' $ zip (toPermutations frames) values

unsafeFromList' :: forall c b a. (HasCallStack, Ord a, Ord b) => [([(a, b)], c)] -> Potential a b c
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
        newValues = V.fromList $ map f (permutationList newFrame)

        p1Frames = M.toAscList p1.frames
        p1Factors = getFactors p1Frames
        p2Frames = M.toAscList p2.frames
        p2Factors = getFactors p2Frames

        f assignment = union (unsafeGetValue' p1 (M.intersection assignment p1.frames) p1Frames p1Factors)
                             (unsafeGetValue' p2 (M.intersection assignment p2.frames) p2Frames p2Factors)

        newFrame = M.unionA p1.frames p2.frames

project :: forall a b c . (Ord a, Ord b) => (c -> c -> c) -> Potential a b c -> Set.Set a  -> Potential a b c
project union p domain = fromPermutationMap projectedToValue
    where

        projectedToValue :: M.Map (M.Map a b) c
        projectedToValue = M.fromListWith union $ zipWith f (permutationList p.frames) (V.toList p.values)

        f permutation value = (M.restrictKeys permutation domain, value)

numPermutations :: M.Map a (S.IndexedSet b) -> Int
numPermutations = product . map S.size . M.elems

-- TODO: There exists a more efficent way to do this.
unsafeGetAssignment :: (Ord a) => M.Map a (S.IndexedSet b) -> Int -> M.Map a b
unsafeGetAssignment frames index = permutationList frames !! index

unsafeGetIndex :: (Ord a, Ord b) => M.Map a b -> M.Map a (S.IndexedSet b) -> Int
unsafeGetIndex assignment frames = unsafeGetIndex' assignment (M.toAscList frames) (getFactors $ M.toAscList frames)

getFactors :: [(a, S.IndexedSet b)] -> [Int]
getFactors frames = tail $ scanr (*) 1 numChoices
    where
        numChoices = map (S.size . snd) frames

-- | Variant of `unsafeGetIndex` that has `M.toAscList` applied to its second parameter and factors precalculated
unsafeGetIndex' :: (Eq a, Ord b) => M.Map a b -> [(a, S.IndexedSet b)] -> [Int] -> Int
unsafeGetIndex' assignment frames factors = sum $ zipWith (*) choiceIndices factors
    where
        choiceIndices = U.zipWithA f (M.toAscList assignment) frames

        f (var, value) (_var, values) = assert (var == _var) $ S.unsafeElemIndex values value

unsafeGetValue :: (Ord a, Ord b) => Potential a b c -> M.Map a b -> c
unsafeGetValue p a = unsafeGetValue' p a (M.toAscList p.frames) (getFactors $ M.toAscList p.frames)

-- | Variant of `unsafeGetValue` that has `M.toAscList` applied to `p.frame` as a parameter and factors precalculated
unsafeGetValue' :: (Ord a, Ord b) => Potential a b c -> M.Map a b -> [(a, S.IndexedSet b)] -> [Int] -> c
unsafeGetValue' p a frames factors = p.values V.! unsafeGetIndex' a frames factors

permutationList :: (Ord a) => M.Map a (S.IndexedSet b) -> [M.Map a b]
permutationList m = map M.fromList $ toPermutations
                                   $ M.toList
                                   $ M.map S.toList
                                   $ m

permutationMap :: (Ord a, Ord b) => Potential a b c -> M.Map (M.Map a b) c
permutationMap p = M.fromList $ zip (permutationList p.frames) (V.toList p.values)

fromPermutationMap :: (Ord a, Ord b) => M.Map (M.Map a b) c -> Potential a b c
fromPermutationMap m = unsafeCreate frames (V.fromList $ M.elems m)
    where
        frames = M.map S.fromSet $ foldr f M.empty $ map (M.map Set.singleton) $ M.keys m

        f = M.unionWith (Set.union)


mapVariables :: forall a1 a2 b c . (Ord a1, Ord a2, Ord b) => (a1 -> a2) -> Potential a1 b c -> Potential a2 b c
mapVariables f p = unsafeCreate newFrames (V.fromList $ M.elems newPermutationMap)
    where
        newFrames = M.mapKeys f p.frames

        newPermutationMap :: M.Map (M.Map a2 b) c
        newPermutationMap = M.mapKeys (M.mapKeys f) $ permutationMap p

-- | Assumes the given function is injective.
mapFrames :: forall a b1 b2 c . (Ord a, Ord b1, Ord b2) => (b1 -> b2) -> Potential a b1 c -> Potential a b2 c
mapFrames f p = unsafeCreate newFrames (V.fromList $ M.elems newPermutationMap)
    where
        newFrames = M.map (S.unsafeMap f) p.frames

        newPermutationMap :: M.Map (M.Map a b2) c
        newPermutationMap = M.mapKeys (M.map f) $ permutationMap p


toFrames :: Potential a b c -> M.Map a (Set.Set b)
toFrames = M.map S.toSet . (.frames)

toValues :: Potential a b c -> [c]
toValues = V.toList . (.values)


null :: Potential a b c -> Bool
null p = M.null p.frames


--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

toPermutations :: [(a, [b])] -> [[(a, b)]]
toPermutations [] = [[]]
toPermutations ((v, possibilites) : others) = [(v, value) : rest
                                               | value <- possibilites, rest <- toPermutations others]

