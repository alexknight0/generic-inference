{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module LocalComputation.Graph
    ( Graph
    , toList
    , isUndirected
    , mapGraph
    , fromList
    , fromList'
    , fromMap
    , toMap
    , Edge (Edge, arcHead, arcTail, weight)
    , nodes
    , flipArcDirections
    )
where

import           Control.Monad (guard)
import qualified Data.Map      as M
import           Data.Maybe    (isJust)
import qualified Data.Set      as S

newtype Graph a b = Graph (M.Map a [(a, b)])
data Edge a b =
    Edge {
          arcHead :: a
        , arcTail :: a
        , weight  :: b
     }

isUndirected :: (Ord a, Eq b) => Graph a b -> Bool
isUndirected (Graph g) = all isJust [f (arcHead, arcTail, cost) | (arcHead, arcTails) <- M.toList g, (arcTail, cost) <- arcTails]
    where
        f (arcHead, arcTail, cost) = do
            arcTailsOfArcTail <- M.lookup arcTail g
            guard $ (arcHead, cost) `elem` arcTailsOfArcTail
            Just ()

mapGraph :: (b -> c) -> Graph a b -> Graph a c
mapGraph f (Graph g) = Graph $ M.map (map (\(x, y) -> (x, f y))) g

toList :: Graph a b -> [Edge a b]
toList (Graph g) = [Edge arcHead arcTail weight | (arcHead, arcTails) <- M.toList g, (arcTail, weight) <- arcTails]

fromList :: forall a b . (Ord a) => [Edge a b] -> Graph a b
fromList = Graph . foldr f M.empty
    where
        f :: Edge a b -> M.Map a [(a, b)] -> M.Map a [(a, b)]
        f e acc = M.insertWith (++) e.arcHead [(e.arcTail, e.weight)] acc

fromList' :: (Ord a) => [(a, [(a, b)])] -> Graph a b
fromList' = Graph . M.fromList

fromMap :: M.Map a [(a, b)] -> Graph a b
fromMap m = Graph m

toMap :: Graph a b -> M.Map a [(a, b)]
toMap (Graph g) = g

nodes :: (Ord a) => Graph a b -> S.Set a
nodes (Graph g) = S.union (M.keysSet g) (S.fromList $ concat $ map (map fst) (M.elems g))

flipArcDirections :: (Ord a) => Graph a b -> Graph a b
flipArcDirections g = fromList [Edge x.arcTail x.arcHead x.weight | x <- toList g]





