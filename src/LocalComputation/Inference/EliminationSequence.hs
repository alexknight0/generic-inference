{-# LANGUAGE ScopedTypeVariables #-}

module LocalComputation.Inference.EliminationSequence
    ( create
    , eliminateNext
    , isEmpty
    , EliminationSequence
    )
where

import qualified Data.Heap         as H
import qualified Data.List         as L
import qualified Data.Map          as M
import qualified Data.Set          as S

import           Control.Exception (assert)

{- | An elimination sequence. Each call to eliminateNext will return the current lowest cost variable to eliminate.

Implemented as a heap. Does not implement the variable valuation linked list (VVLL) described page 25 of
"A generic Architecture for local Computation" (Marc Pouly) and hence performance could easily be improved.
-}
newtype EliminationSequence a = EliminationSequence (H.MinPrioHeap (OrderByLength S.Set a) a)

data StructureError a = DuplicateVariable | VariableNotMemberOfOwnClique a

{- | Returns 'Right ()' if the elimination sequence is well formed, otherwise indicates the error with the structure. -}
isWellFormed :: (Ord a) => EliminationSequence a -> Either (StructureError a) ()
isWellFormed (EliminationSequence xs)
    | Just (_, y) <- L.find p xs' = Left (VariableNotMemberOfOwnClique y)
    | length (S.fromList xs') /= length xs' = Left DuplicateVariable
    | otherwise = Right ()
    where
        xs' = H.toList xs
        p (clique, x) = x `notElem` clique

-- | Creates an elimination sequence from a given set of domains.
create :: (Ord a) => [S.Set a] -> EliminationSequence a
create xs = EliminationSequence $ H.fromList $ map (\(var, clique) -> (OrderByLength clique, var)) (M.toList (getCliques xs))

-- | Returns true if there are no variables left to eliminate.
isEmpty :: EliminationSequence a -> Bool
isEmpty (EliminationSequence xs) = H.isEmpty xs

getCliques :: (Ord a) => [S.Set a] -> M.Map a (S.Set a)
getCliques xs = M.unionsWith S.union $ map f xs
    where
        f :: (Ord a) => S.Set a -> M.Map a (S.Set a)
        f ys = foldr g (M.empty) ys
            where
                g y acc = M.insert y ys acc

eliminateNext :: (Ord a) => EliminationSequence a -> Maybe (a, EliminationSequence a)
eliminateNext xs | assertIsWellFormed xs = undefined
eliminateNext (EliminationSequence xs)
    | Just ((_, x), xs') <- H.view xs = Just (x, EliminationSequence $ removeFromAllCliques x xs')
    | otherwise = Nothing
    where
        removeFromAllCliques :: (Ord a) => a -> H.MinPrioHeap (OrderByLength S.Set a) a -> H.MinPrioHeap (OrderByLength S.Set a) a
        removeFromAllCliques x ys = H.fromList $ map f $ H.toList ys
            where
                f (OrderByLength clique, y) = (OrderByLength $ S.delete x clique, y)

newtype OrderByLength f a = OrderByLength (f a)

instance (Foldable f) => Foldable (OrderByLength f) where
    foldr f acc (OrderByLength xs) = foldr f acc xs

instance (Foldable f) => Eq (OrderByLength f a) where
    xs == ys = length xs == length ys

instance (Foldable f) => Ord (OrderByLength f a) where
    xs <= ys = length xs <= length ys

assertIsWellFormed :: (Ord a) => EliminationSequence a -> Bool
assertIsWellFormed xs = assert p False
    where
        p = case isWellFormed xs of
                Left _   -> False
                Right () -> True
