{-# LANGUAGE ScopedTypeVariables #-}

module LocalComputation.Inference.EliminationSequence
    ( create
    , createAndExclude
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
--
-- prop> create ds = createAndExclude ds S.empty
create :: (Ord a) => [S.Set a] -> EliminationSequence a
create ds = createAndExclude ds S.empty

-- | Creates an elimination sequence from a given set of domains, then excludes a set of
-- given variables from occuring within the sequence of elimination.
--
-- i.e. while the excluded variables will be counted in the cliques of other variables,
-- they won't be in the elimination sequence produced by repeated calls to 'eliminateNext'
createAndExclude :: (Ord a) => [S.Set a] -> S.Set a -> EliminationSequence a
createAndExclude ds excluded = EliminationSequence $ H.fromList
                                                   $ map (\(var, clique) -> (OrderByLength clique, var))
                                                   $ M.toList
                                                   $ (`M.withoutKeys` excluded)
                                                   $ getCliques ds

-- | Returns true if there are no variables left to eliminate.
isEmpty :: EliminationSequence a -> Bool
isEmpty (EliminationSequence xs) = H.isEmpty xs

getCliques :: (Ord a) => [S.Set a] -> M.Map a (S.Set a)
getCliques ds = M.unionsWith S.union $ map f ds
    where
        f :: (Ord a) => S.Set a -> M.Map a (S.Set a)
        f ys = foldr g (M.empty) ys
            where
                g y acc = M.insert y ys acc

eliminateNext :: (Ord a) => EliminationSequence a -> Maybe (a, EliminationSequence a)
eliminateNext vars | assertIsWellFormed vars = undefined
eliminateNext (EliminationSequence vars)
    | Just ((_, var), vars') <- H.view vars = Just (var, EliminationSequence $ removeFromAllCliques var vars')
    | otherwise = Nothing
    where
        -- Seems like may get rid of the benefit of using a heap; if performance is an issue
        -- look further into how a heap is supposed to be utilised here.
        removeFromAllCliques :: (Ord a) => a -> H.MinPrioHeap (OrderByLength S.Set a) a -> H.MinPrioHeap (OrderByLength S.Set a) a
        removeFromAllCliques var otherVars = H.fromList $ map f $ H.toList otherVars
            where
                f (OrderByLength clique, y) = (OrderByLength $ S.delete var clique, y)

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
