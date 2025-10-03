{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
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
    , vertexCount
    , edgeCount
    , addSelfLoops
    , hasZeroCostSelfLoops
    , nonSymmetricEdges
    , merge
    , merges
    , merges1
    , adjacencyList
    , minimiseEdgeCosts
    , reverseAdjacencyList
    , empty
    , isEmpty
    , deleteVertex
    , isConnected
    , outgoingSubgraph
    , neighbours
    , induce
    , toAlgebraGraph
    , toAlgebraGraph'
    )
where

import qualified Algebra.Graph              as G
import qualified Algebra.Graph.Undirected   as UG
import           Control.Monad              (guard)
import qualified Data.List                  as L
import qualified Data.Map                   as M
import           Data.Maybe                 (isJust)
import qualified Data.Set                   as S
import qualified Data.Text.Lazy             as LT
import           Text.Pretty.Simple         (pShowNoColor)

import qualified Algebra.Graph.AdjacencyMap as AM
import qualified Algebra.Graph.ToGraph      as AM
import           Control.DeepSeq            (NFData)
import           Control.Exception          (assert)
import           GHC.Generics               (Generic)
import qualified LocalComputation.Utils     as U

newtype Graph a b = Graph (M.Map a [(a, b)]) deriving (Generic, NFData, Eq)

instance (Show a, Show b) => Show (Graph a b) where
    show (Graph g) = LT.unpack $ pShowNoColor g

data Edge a b = Edge {
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

-- TODO: Inefficent, but not used for anything important
edgeCount :: Graph a b -> Int
edgeCount = length . toList

fromMap :: M.Map a [(a, b)] -> Graph a b
fromMap m = Graph m

toMap :: Graph a b -> M.Map a [(a, b)]
toMap (Graph g) = g

-- TODO: Rename to vertexSet and vertexList

-- TODO: The performance of this operation could be improved by adding an invariant that the underlying map contains
-- every node in it's keySet, regardless of whether the key has an arc leaving from it.
nodes :: (Ord a) => Graph a b -> S.Set a
nodes (Graph g) = S.union (M.keysSet g) (S.fromList $ concat $ map (map fst) (M.elems g))

nodeList :: (Ord a) => Graph a b -> [a]
nodeList g = S.toList $ nodes g

vertexCount :: (Ord a) => Graph a b -> Int
vertexCount = length . nodeList

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

toAlgebraGraph :: Graph a b -> G.Graph a
toAlgebraGraph g = G.overlays [G.edge e.arcHead e.arcTail | e <- toList g]

toAlgebraGraph' :: Graph a b -> UG.Graph a
toAlgebraGraph' = UG.toUndirected . toAlgebraGraph

-- TODO: Very inefficent
neighbours :: (Ord a) => Graph a b -> [(a, [a])]
neighbours = UG.adjacencyList . UG.toUndirected . toAlgebraGraph

-- TODO: Stupidly complicated because the keys dont necessarily represent the full number of vertices...
-- The whole data structure should be reworked.
adjacencyList :: (Ord a) => Graph a b -> [(a, [a])]
adjacencyList g = G.adjacencyList $ toAlgebraGraph g

reverseAdjacencyList :: (Ord a) => Graph a b -> [(a, [a])]
reverseAdjacencyList = adjacencyList . flipArcDirections

-- | Returns a subgraph of the given graph that includes only
-- vertices from the given set and those vertices' outgoing edges.
outgoingSubgraph :: (Ord a) => Graph a b -> S.Set a -> Graph a b
outgoingSubgraph (Graph g) new = fromList' $ filter (\(vertex, _) -> S.member vertex new)
                                           $ M.toList g

empty :: Graph a b
empty = Graph M.empty

isEmpty :: Graph a b -> Bool
isEmpty (Graph g) = length g == 0

deleteVertex :: (Ord a) => a -> Graph a b -> Graph a b
deleteVertex x g = fromList $ filter (\e -> e.arcHead /= x
                                         && e.arcTail /= x) $ toList g

deleteVertices :: (Ord a) => S.Set a -> Graph a b -> Graph a b
deleteVertices xs g = fromList $ filter (\e -> e.arcHead `S.notMember` xs
                                            && e.arcTail `S.notMember` xs) $ toList g

makeUndirected :: (Ord a) => G.Graph a -> G.Graph a
makeUndirected g = AM.toGraph $ AM.overlay g' (AM.transpose g')
    where
        g' = AM.toAdjacencyMap g

isConnected :: (Ord a) => Graph a b -> Bool
isConnected g = all (\x -> length (AM.reachable undirected x) == length vertices) vertices
    where
        undirected = makeUndirected $ toAlgebraGraph g
        vertices = G.vertexList $ toAlgebraGraph g

induce :: (Ord a) => (a -> Bool) -> Graph a b -> Graph a b
induce p g =  deleteVertices (S.filter (not . p) (nodes g)) g

minimiseEdgeCosts :: (Ord a, Ord b) => Graph a b -> Graph a b
minimiseEdgeCosts (Graph m) = fromMap $ M.map (\adj -> U.nubWithBy fst minimise adj) m
    where
        minimise (x1, c1) (x2, c2) = assert (x1 == x2) $ (x1, min c1 c2)
