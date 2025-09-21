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
import qualified Data.IntMap                       as IntMap
import qualified Data.List                         as L
import qualified Data.Map                          as M
import           Data.Maybe                        (fromJust)
import qualified Data.Set                          as Set
import           GHC.Stack                         (HasCallStack)
import qualified LocalComputation.IndexableSet     as S
import qualified LocalComputation.Utils            as M (unionA)
import qualified LocalComputation.Utils            as U
import           LocalComputation.ValuationAlgebra (Binary, Generic, NFData)
import           Prelude                           hiding (null)


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
data Potential a b c = Potential { frames :: M.Map a (S.IndexableSet b), values :: [c] }
                                    deriving (Binary, Generic, NFData)

unsafeCreate :: HasCallStack => M.Map a (S.IndexableSet b) -> [c] -> Potential a b c
unsafeCreate frames values = U.assertP satisfiesInvariants $ Potential frames values

unsafeCreate' :: Ord b => M.Map a (Set.Set b) -> [c] -> Potential a b c
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
        newValues = map f (permutationList newFrame)

        f assignment = union (unsafeGetValue p1 (M.intersection assignment p1.frames))
                             (unsafeGetValue p2 (M.intersection assignment p2.frames))

        newFrame = M.unionA p1.frames p2.frames

project :: forall a b c . (Ord a, Ord b) => (c -> c -> c) -> Potential a b c -> Set.Set a  -> Potential a b c
project union p domain = fromPermutationMap projectedToValue
    where

        projectedToValue :: M.Map (M.Map a b) c
        projectedToValue = M.fromListWith union $ zipWith f (permutationList p.frames) p.values

        f permutation value = (M.restrictKeys permutation domain, value)

numPermutations :: M.Map a (S.IndexableSet b) -> Int
numPermutations = product . map S.size . M.elems

-- TODO: There exists a more efficent way to do this.
unsafeGetAssignment :: (Ord a) => M.Map a (S.IndexableSet b) -> Int -> M.Map a b
unsafeGetAssignment frames index = permutationList frames !! index

unsafeGetIndex :: (Ord a, Eq b) => M.Map a b -> M.Map a (S.IndexableSet b) -> Int
unsafeGetIndex assignment frames = sum $ zipWith (*) choiceIndices factors
    where
        choiceIndices = U.zipWithA f (M.toAscList assignment) (M.toAscList frames)

        numChoices = map (S.size . snd) $ M.toAscList frames

        factors = tail $ scanr (*) 1 numChoices

        f (var, value) (_var, values) = assert (var == _var) $ S.unsafeElemIndex values value

unsafeGetValue :: (Ord a, Eq b) => Potential a b c -> M.Map a b -> c
unsafeGetValue p a = p.values !! unsafeGetIndex a p.frames

permutationList :: (Ord a) => M.Map a (S.IndexableSet b) -> [M.Map a b]
permutationList m = map M.fromList $ toPermutations
                                   $ M.toList
                                   $ M.map S.toList
                                   $ m

permutationMap :: (Ord a, Ord b) => Potential a b c -> M.Map (M.Map a b) c
permutationMap p = M.fromList $ zip (permutationList p.frames) p.values

fromPermutationMap :: (Ord a, Ord b) => M.Map (M.Map a b) c -> Potential a b c
fromPermutationMap m = unsafeCreate frames (M.elems m)
    where
        frames = M.map S.fromSet $ foldr f M.empty $ map (M.map Set.singleton) $ M.keys m

        f = M.unionWith (Set.union)


mapVariables :: forall a1 a2 b c . (Ord a1, Ord a2, Ord b) => (a1 -> a2) -> Potential a1 b c -> Potential a2 b c
mapVariables f p = unsafeCreate newFrames $ M.elems newPermutationMap
    where
        newFrames = M.mapKeys f p.frames

        newPermutationMap :: M.Map (M.Map a2 b) c
        newPermutationMap = M.mapKeys (M.mapKeys f) $ permutationMap p

-- | Assumes the given function is injective.
mapFrames :: forall a b1 b2 c . (Ord a, Ord b1, Ord b2) => (b1 -> b2) -> Potential a b1 c -> Potential a b2 c
mapFrames f p = unsafeCreate newFrames $ M.elems newPermutationMap
    where
        newFrames = M.map (S.map f) p.frames

        newPermutationMap :: M.Map (M.Map a b2) c
        newPermutationMap = M.mapKeys (M.map f) $ permutationMap p


toFrames :: (Eq b) => Potential a b c -> M.Map a (Set.Set b)
toFrames = M.map S.toSet . (.frames)

toValues :: Potential a b c -> [c]
toValues = (.values)


null :: Potential a b c -> Bool
null p = M.null p.frames


--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

toPermutations :: [(a, [b])] -> [[(a, b)]]
toPermutations [] = [[]]
toPermutations ((v, possibilites) : others) = [(v, value) : rest
                                               | value <- possibilites, rest <- toPermutations others]

