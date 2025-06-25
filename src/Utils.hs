{-# LANGUAGE ScopedTypeVariables #-}

module Utils
    ( setMap, nubWithBy, thd4, snd4, fth4, findAssertSingleMatch, unsafeFind, unionUnsafe, fromListAssertDisjoint, unionAssertDisjoint )
where

import Data.List (find)
import Data.Set (Set, map, intersection)
import Data.Map (insert, adjust, Map, elems, member)
import qualified Data.Map as M

setMap :: (Ord b) => (a -> b) -> Set a -> Set b
setMap f xs = Data.Set.map f xs

nubWithBy :: forall a b . (Ord b) => (a -> b) -> (a -> a -> a) -> [a] -> [a]
nubWithBy toKey f xs = elems $ foldr g M.empty xs
    where
        g :: a -> Map b a -> Map b a
        g x acc
            | toKey x `member` acc = adjust (\y -> f x y) (toKey x) acc
            | otherwise = insert (toKey x) x acc


thd4 :: (a, b, c, d) -> c
thd4 (_, _, x, _) = x

snd4 :: (a, b, c, d) -> b
snd4 (_, x, _, _) = x

fth4 :: (a, b, c, d) -> d
fth4 (_, _, _, x) = x

normalize :: (Fractional a) => [a] -> [a]
normalize xs = fmap (/ sumXs) xs
    where
        sumXs = sum xs

findAssertSingleMatch :: (a -> Bool) -> [a] -> a
findAssertSingleMatch p xs
    | [y] <- filter p xs = y
    | ys <- filter p xs = let numMatches = length (take 10000 ys) in
                                error $ "findAssertSingleMatch found " ++ (if numMatches == 10000 then ">" else "") ++ show numMatches ++ " matches"

unsafeFind :: Foldable t => (a -> Bool) -> t a -> a
unsafeFind p xs
    | (Just y) <- Data.List.find p xs = y
    | otherwise = error "unsafeFind found nothing"

unionUnsafe :: (Eq b, Ord a) => Map a b -> Map a b -> Map a b
unionUnsafe = M.unionWith (\x y -> if x /= y then error "Map keys that exist in both maps do not have the same value" else x)

unionAssertDisjoint :: (Ord a) => Map a b -> Map a b -> Map a b
unionAssertDisjoint = M.unionWith (\_ _ -> error "Map key sets are not disjoint")

fromListAssertDisjoint :: (Ord a) => [(a, b)] -> Map a b
fromListAssertDisjoint = M.fromListWith (\_ _ -> error "Attempted to create map from non disjoint assoc list")
