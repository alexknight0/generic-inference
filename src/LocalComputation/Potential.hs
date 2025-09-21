{-# LANGUAGE ScopedTypeVariables #-}
module LocalComputation.Potential (

    Potential
  , toFrames
  , toValues
  , project
  , combine
  , unsafeCreate
  , unsafeFromList



) where

import           Control.Exception             (assert)
import qualified Data.IntMap                   as IntMap
import qualified Data.List                     as L
import qualified Data.Map                      as M
import           Data.Maybe                    (fromJust)
import qualified Data.Set                      as Set
import qualified LocalComputation.IndexableSet as S
import qualified LocalComputation.Utils        as M (unionA)
import qualified LocalComputation.Utils        as U


data Potential a b c = Potential { frames :: M.Map a (S.IndexableSet b), values :: [c] }

unsafeCreate frames values = U.assertP satisfiesInvariants $ Potential frames values

-- TODO: !!! Order of 'a' and 'b' becomes order of 'c'...
unsafeFromList :: forall c b a. (Ord a, Ord b) => [(a, [b])] -> [c] -> Potential a b c
unsafeFromList frames values = unsafeCreate (M.map (S.unsafeFromList frames) $ M.fromList frames) values

{- | Satisfies invariants if:
    1. The number of permutations is equal to the number of values
-}
satisfiesInvariants :: (Ord b) => Potential a b c -> Bool
satisfiesInvariants p = numPermutations p.frames == length p.values -- (1)

combine :: (Ord a, Ord b) => (c -> c -> c) -> Potential a b c -> Potential a b c -> Potential a b c
combine union p1 p2 = unsafeCreate newFrame newValues
    where
        newValues = map f (permutationList newFrame)

        f assignment = union (p1.values !! unsafeGetIndex (M.intersection assignment p1.frames) p1.frames)
                             (p2.values !! unsafeGetIndex (M.intersection assignment p2.frames) p2.frames)

        newFrame = M.unionA p1.frames p2.frames

project :: forall a b c . (Ord a, Ord b) => (c -> c -> c) -> Potential a b c -> Set.Set a  -> Potential a b c
project union p domain = unsafeCreate newFrame (IntMap.elems projectedToIndex)
    where
        projectedToIndex :: IntMap.IntMap c
        projectedToIndex = IntMap.fromList $ map g $ M.toList projectedToValue

        -- TODO: `unsafeGetIndex` might be unnecessary due to natural ordering of permutations
        -- provided we set the right order during combine in `unsafeGetIndex`.
        g (permutation, value) = (unsafeGetIndex permutation newFrame, value)

        projectedToValue :: M.Map (M.Map a b) c
        projectedToValue = M.fromListWith union $ zipWith f (permutationList p.frames) p.values

        f permutation value = (M.restrictKeys permutation domain, value)

        newFrame = M.restrictKeys p.frames domain

numPermutations :: M.Map a (S.IndexableSet b) -> Int
numPermutations = product . map S.size . M.elems

unsafeGetIndex :: (Ord a, Eq b) => M.Map a b -> M.Map a (S.IndexableSet b) -> Int
unsafeGetIndex assignment frames = sum $ zipWith (*) indices [2 ^ i | i :: Integer <- [0..]]
    where
        indices = U.zipWithA f (M.toAscList assignment) (M.toAscList frames)

        f (var, value) (_var, values) = assert (var == _var) $ S.unsafeElemIndex values value

-- unsafeGetIndex' :: (Ord a, Eq b) => (a, b) -> Potential a b c -> Int
-- unsafeGetIndex' (var, value) p = fromJust $ L.elemIndex value $ (M.!) p.frames var

permutationList :: (Ord a) => M.Map a (S.IndexableSet b) -> [M.Map a b]
permutationList m = map M.fromList $ permutations (M.toList $ M.map S.toList m)
    where
        permutations :: [(a, [b])] -> [[(a, b)]]
        permutations [] = [[]]
        permutations ((v, possibilites) : others) = [(v, value) : rest
                                                       | value <- possibilites, rest <- permutations others]

toFrames :: Potential a b c -> M.Map a (S.IndexableSet b)
toFrames = (.frames)

toValues :: Potential a b c -> [c]
toValues = (.values)



