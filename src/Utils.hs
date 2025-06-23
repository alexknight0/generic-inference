{-# LANGUAGE ScopedTypeVariables #-}

module Utils
    ( setMap, nubWithBy, thd4, snd4, fth4)
where

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
