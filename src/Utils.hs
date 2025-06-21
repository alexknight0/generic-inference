module Utils
    ( setMap )
where

import Data.Set (Set, fromList, toList)

setMap :: (Ord b) => (a -> b) -> Set a -> Set b
setMap f xs = fromList $ map f (toList xs)
