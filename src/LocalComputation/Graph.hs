{-# LANGUAGE ScopedTypeVariables #-}

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
    , nodeList
    , addSelfLoops
    , hasZeroCostSelfLoops
    , nonSymmetricEdges
    )
where

import           Control.Monad (guard)
import qualified Data.List     as L
import qualified Data.Map      as M
import           Data.Maybe    (isJust)
import qualified Data.Set      as S

newtype Graph a b = Graph (M.Map a [(a, b)]) deriving Show
data Edge a b =
    Edge {
          arcHead :: a
        , arcTail :: a
        , weight  :: b
     } deriving Show

instance Functor (Graph a) where
    fmap f (Graph g) = Graph $ M.map (map (fmap f)) g

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

-- TODO: The performance of this operation could be improved by adding an invariant that the underlying map contains
-- every node in it's keySet, regardless of whether the key has an arc leaving from it.
nodes :: (Ord a) => Graph a b -> S.Set a
nodes (Graph g) = S.union (M.keysSet g) (S.fromList $ concat $ map (map fst) (M.elems g))

nodeList :: (Ord a) => Graph a b -> [a]
nodeList g = S.toList $ nodes g

flipArcDirections :: (Ord a) => Graph a b -> Graph a b
flipArcDirections g = fromList [Edge x.arcTail x.arcHead x.weight | x <- toList g]

{- | Adds self loops of the given weight to each node of a graph. -}
addSelfLoops :: (Ord a) => b -> Graph a b -> Graph a b
addSelfLoops b (Graph g) = Graph $ M.unionWith (++) g (M.fromList [(node, [(node, b)]) | node <- nodeList (Graph g)])

hasZeroCostSelfLoops :: (Ord a, Eq b, Num b) => Graph a b -> Bool
hasZeroCostSelfLoops (Graph g) = all p (nodeList $ Graph g)
    where
        p arcHead
            | Just arcTails <- M.lookup arcHead g, (arcHead, 0) `elem` arcTails = True
            | otherwise = False

-- | Returns a list of all the edges in a that don't have an opposite edge of the same cost.
-- This function is not as performant as it could be.
nonSymmetricEdges :: forall a b . (Eq a, Eq b) => Graph a b -> [Edge a b]
nonSymmetricEdges g = filter f edges
    where
        f :: Edge a b -> Bool
        f e1
            | (Just _) <- L.find (\e2 -> isOppositeEdge e1 e2 && e1.weight == e2.weight) edges = False
            | otherwise = True

        edges = toList g

isOppositeEdge :: (Eq a) => Edge a b -> Edge a b -> Bool
isOppositeEdge e1 e2 = e1.arcTail == e2.arcHead && e1.arcHead == e2.arcTail

