{-# LANGUAGE ScopedTypeVariables #-}

module LocalComputation.Inference.EliminationSequence
    ( create
    , createAndExclude
    , eliminateNext
    , isEmpty
    , size
    , EliminationSequence
    )
where

import qualified Data.List              as L
import qualified Data.Map               as M
import           Data.PSQueue           (Binding ((:->)))
import qualified Data.PSQueue           as P
import qualified Data.Set               as S

import           Data.Either            (isRight)
import qualified LocalComputation.Utils as U

{- | An elimination sequence. Each call to eliminateNext will return the current lowest cost variable to eliminate.

Implemented as a heap. Does not implement the variable valuation linked list (VVLL) described page 25 of
"A generic Architecture for local Computation" (Marc Pouly) and hence performance could be improved.

Finding the 'clique' of a given variable is taking the set of valuations that contain that variable and then
unioning all of their variables together.
-}
data EliminationSequence a = EliminationSequence { pq :: P.PSQ a (OrderByLength S.Set a)
                                                    , vvl :: M.Map a (S.Set a)
                                                } deriving Show

unsafeCreate :: (Ord a) => P.PSQ a (OrderByLength S.Set a) -> M.Map a (S.Set a) -> EliminationSequence a
unsafeCreate pq vvl = U.assertP (isRight . isWellFormed) $ EliminationSequence pq vvl

data StructureError a = DuplicateVariable | VariableNotMemberOfOwnClique a

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
createAndExclude ds excluded = unsafeCreate pq vvl
    where
        pq = P.fromList $ map (\(var, clique) -> var :-> OrderByLength clique)
                        $ M.toList
                        $ (`M.withoutKeys` excluded)
                        $ getCliques ds

        cliques = getCliques ds
        vvl = vvlFromCliques cliques


-- | Returns true if there are no variables left to eliminate.
isEmpty :: EliminationSequence a -> Bool
isEmpty (EliminationSequence xs _) = P.null xs

size :: EliminationSequence a -> Int
size (EliminationSequence xs _) = P.size xs

vvlFromCliques :: (Ord a) => M.Map a (S.Set a) -> M.Map a (S.Set a)
vvlFromCliques cliques = M.unions $ map vvlFromEntry $ M.toList cliques
    where

        vvlFromEntry :: (Eq a) => (a, S.Set a) -> M.Map a (S.Set a)
        vvlFromEntry (var, clique) = M.fromAscList $ map (\v -> (v, S.singleton var)) $ S.toAscList clique



getCliques :: (Ord a) => [S.Set a] -> M.Map a (S.Set a)
getCliques ds = M.unionsWith S.union $ map toVarCliques ds
    where
        -- Takes a domain and returns a map where each element of the map points back to that domain, i.e.
        -- toVarCliques {1,2} = 1 -> {1,2}
        --                      2 -> {1,2}
        toVarCliques :: (Ord a) => S.Set a -> M.Map a (S.Set a)
        toVarCliques d = foldr g M.empty d
            where
                g variable acc = M.insert variable d acc

eliminateNext :: (Ord a) => EliminationSequence a -> Maybe (a, EliminationSequence a)
eliminateNext e = do
    (eliminated :-> _, pqAfterElimination) <- P.minView e.pq

    let newPQ = adjusts updatePriority varsWhoseCliqueChanged pqAfterElimination
        updatePriority (OrderByLength c) = OrderByLength $ S.delete eliminated c
        varsWhoseCliqueChanged = S.toList $ (M.!) e.vvl eliminated

    pure $ (eliminated, unsafeCreate newPQ newVVL)

    where
        -- The new vvl could be updated by removing a key, but as we don't care
        -- too much about the memory use, it's probably better to save the computation.
        newVVL = e.vvl


--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------
adjusts :: (Ord k, Ord p) => (p -> p) -> [k] -> P.PSQ k p -> P.PSQ k p
adjusts p ks pq = foldr (P.adjust p) pq ks





newtype OrderByLength f a = OrderByLength (f a) deriving Show

instance (Foldable f) => Foldable (OrderByLength f) where
    foldr f acc (OrderByLength xs) = foldr f acc xs

instance (Foldable f) => Eq (OrderByLength f a) where
    xs == ys = length xs == length ys

instance (Foldable f) => Ord (OrderByLength f a) where
    xs <= ys = length xs <= length ys

--------------------------------------------------------------------------------
-- Invariants
--------------------------------------------------------------------------------
{- | Returns 'Right ()' if the elimination sequence is well formed, otherwise indicates the error with the structure. -}
isWellFormed :: (Ord a) => EliminationSequence a -> Either (StructureError a) ()
isWellFormed (EliminationSequence xs _)
    | Just (y :-> _) <- L.find p xs'        = Left (VariableNotMemberOfOwnClique y)
    | length (S.fromList xs') /= length xs' = Left DuplicateVariable
    | otherwise                             = Right ()
    where
        xs' = P.toList xs
        p (x :-> clique) = x `notElem` clique

