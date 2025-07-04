{-# LANGUAGE ScopedTypeVariables #-}

module LabelledMatrix
    ( LabelledMatrix
    , fromMatrix
    , fromList
    , identity
    , extension
    , project
    , find
    , add
    , multiply
    )
where

import qualified Data.Map    as M
import qualified Data.Matrix as M'
import           Data.Maybe  (fromJust, isJust)
import qualified Data.Set    as S
import           Utils

{- | A labelled matrix. -}
data LabelledMatrix a b c = Matrix (M.Map (a, b) c) (S.Set a) (S.Set b)

instance (Eq a, Eq b, Eq c) => Eq (LabelledMatrix a b c) where
    (Matrix m1 _ _) == (Matrix m2 _ _) = m1 == m2

instance (Show a, Show b, Show c) => Show (LabelledMatrix a b c) where
    show (Matrix m1 _ _) = show m1

-- | Transforms a regular matrix from Data.Matrix into a matrix labelled by Integers.
fromMatrix :: M'.Matrix a -> LabelledMatrix Integer Integer a
fromMatrix m = fromList $ concat $ zipWith (\i row -> zipWith (\j x -> ((i, j), x)) [0..] row) [0..] (M'.toLists m)

-- | Creates a matrix from the given association list. Unsafe - throws errors if the input list is invalid.
fromList :: forall a b c . (Ord a, Ord b)
    => [((a, b), c)]
    -> LabelledMatrix a b c
fromList xs
    | length as * length bs /= length xs = error "Not a total mapping - some values are missing."
    | otherwise = Matrix (M.fromListWith (\_ _ -> error "Duplicate key in matrix creation assoc list.") xs) as bs
    where
        as = S.fromList $ map (\((x, _), _) -> x) xs
        bs = S.fromList $ map (\((_, x), _) -> x) xs

-- | Returns the identity matrix created with the given zero and one elements.
identity :: (Ord a) => S.Set a -> c -> c -> LabelledMatrix a a c
identity dA zero one = fromList [((x, y), if x == y then one else zero) | x <- as, y <- as]
    where
        as = S.toList dA

-- | Extend a matrix to a larger domain, filling spots with the given zero element. Returns Nothing if the domain to extend to is not a superset.
extension :: (Ord a, Ord b) => LabelledMatrix a b c -> S.Set a -> S.Set b -> c -> Maybe (LabelledMatrix a b c)
extension (Matrix m dA dB) newDA newDB zero
    | newDA `S.isProperSubsetOf` dA || newDB `S.isProperSubsetOf` dB = Nothing
    | otherwise = Just $ Matrix (m `M.union` mapOfZeroes) newDA newDB
    where
        mapOfZeroes = M.fromList [((a, b), zero) | a <- S.toList newDA, b <- S.toList newDB]

-- | Project the domain of a matrix down to a new domain. Returns nothing if the given domain is not a subset of the old domain.
project :: (Ord a, Ord b) => LabelledMatrix a b c -> S.Set a -> S.Set b -> Maybe (LabelledMatrix a b c)
project (Matrix m dA dB) newDA newDB
    | newDA `S.isSubsetOf` dA && newDB `S.isSubsetOf` dB = Just $ Matrix (M.filterWithKey (\(a, b) _ -> a `elem` newDA && b `elem` newDB) m) newDA newDB
    | otherwise = Nothing

-- | Returns an element from the matrix. Returns Nothing if the element is not in the domain of the matrix.
find :: (Ord a, Ord b) => (a, b) -> LabelledMatrix a b c -> Maybe c
find (a, b) (Matrix m dA dB)
    | a `elem` dA && b `elem` dB = assert' isJust $ M.lookup (a, b) m
    | otherwise = Nothing

-- | Basic addition on two matrices. Returns Nothing if the provided matrices have different shapes.
add :: (Ord a, Ord b) => (c -> c -> c) -> LabelledMatrix a b c -> LabelledMatrix a b c -> Maybe (LabelledMatrix a b c)
add addElems (Matrix m1 dA1 dB1) (Matrix m2 dA2 dB2)
    | dA1 /= dA2 || dB1 /= dB2 = Nothing
    | otherwise = Just $ Matrix (M.intersectionWith addElems m1 m2) dA1 dB1

-- | Basic matrix multiplication on two matrices. Returns Nothing if the provided matrices have the wrong shape for matrix multiplication.
multiply :: forall a b c d . (Ord a, Ord b, Ord c) => (d -> d -> d) -> (d -> d -> d) -> LabelledMatrix a b d -> LabelledMatrix b c d -> Maybe (LabelledMatrix a c d)
multiply addElems multiplyElems m1@(Matrix _ dA dB1) m2@(Matrix _ dB2 dC)
    | dB1 /= dB2 = Nothing
    | otherwise = Just $ Matrix (fromListAssertDisjoint newAssocList) dA dC
    where
        (as, bs, cs) = (S.toList dA, S.toList dB1, S.toList dC)

        newAssocList = [(  (a, c),
                           foldr1 addElems [fromJust (find (a, b) m1) `multiplyElems` fromJust (find (b, c) m2) | b <- bs]
                        ) | a <- as, c <- cs]

