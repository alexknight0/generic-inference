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
    , merge
    , merges
    , merges1
    , adjacencyList
    , reverseAdjacencyList
    , empty
    )
where

import qualified Algebra.Graph      as AG
import           Control.Monad      (guard)
import qualified Data.List          as L
import qualified Data.Map           as M
import           Data.Maybe         (isJust)
import qualified Data.Set           as S
import qualified Data.Text.Lazy     as LT
import           Text.Pretty.Simple (pShow, pShowNoColor)

newtype Graph a b = Graph (M.Map a [(a, b)])

instance (Show a, Show b) => Show (Graph a b) where
    show (Graph g) = LT.unpack $ pShowNoColor g
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
addSelfLoops b g = merge g selfLoops
    where
        selfLoops = Graph $ M.fromList [(node, [(node, b)]) | node <- nodeList g]

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

-- | Merges the two given graphs.
merge :: (Ord a) => Graph a b -> Graph a b -> Graph a b
merge (Graph g1) (Graph g2) = Graph $ M.unionWith (++) g1 g2

merges :: (Foldable f, Ord a) => Graph a b -> f (Graph a b) -> Graph a b
merges initial gs = foldr merge initial gs

merges1 :: (Foldable f, Ord a) => f (Graph a b) -> Graph a b
merges1 gs = foldr1 merge gs

toAlgebraGraph :: Graph a b -> AG.Graph a
toAlgebraGraph g = AG.overlays [AG.edge e.arcHead e.arcTail | e <- toList g]

adjacencyList :: (Ord a) => Graph a b -> [(a, [a])]
adjacencyList g = AG.adjacencyList $ toAlgebraGraph g

reverseAdjacencyList :: (Ord a) => Graph a b -> [(a, [a])]
reverseAdjacencyList = adjacencyList . flipArcDirections

empty :: Graph a b
empty = Graph M.empty
