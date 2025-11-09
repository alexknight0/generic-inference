{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LocalComputation.Graph
    ( Graph
    , toEdgeList
    , isUndirected
    , mapCosts
    , fromList
    , fromList'
    , unsafeFromMap
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
    , reverseAdjacencyMap
    , empty
    , isEmpty
    , isConnected
    , outgoingSubgraph
    , neighbourMap
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

newtype Graph a b = Graph { m :: M.Map a [(a, b)] } deriving (Generic, NFData, Eq)

-- A map is a valid graph if:
--   1. All vertices appearing as a edge tail appear in the keys set of the map.
unsafeFromMap :: (Ord a) => M.Map a [(a, b)] -> Graph a b
unsafeFromMap = U.assertP satisfiesInvariants . Graph


satisfiesInvariants :: (Ord a) => Graph a b -> Bool
satisfiesInvariants g = all (`S.member` M.keysSet g.m) edgeTails  -- All vertices appearing as a edge tail
    where
        edgeTails = L.nub $ concat $ map (map fst) (M.elems g.m)

instance (Show a, Show b) => Show (Graph a b) where
    show g = LT.unpack $ pShowNoColor g.m

data Edge a b = Edge {
      arcHead :: a
    , arcTail :: a
    , weight  :: b
 } deriving Show

instance (Ord a) => Functor (Graph a) where
    fmap f g = unsafeFromMap $ M.map (map (fmap f)) g.m

toMap :: Graph a b -> M.Map a [(a, b)]
toMap = (.m)

fromList' :: (Ord a) => [(a, [(a, b)])] -> Graph a b
fromList' xs = unsafeFromMap mapWithProperVertices
    where
        -- Ensures each vertex appearing in an edge tail appears in the key set of the map.
        mapWithProperVertices = foldr (M.alter f) baseMap edgeTails

        f :: Maybe [(a, b)] -> Maybe [(a, b)]
        f Nothing = Just []
        f x       = x

        baseMap = M.fromList xs
        edgeTails = L.nub $ concat $ map (map fst . snd) xs

isUndirected :: (Ord a, Eq b) => Graph a b -> Bool
isUndirected (Graph g) = all isJust [f (arcHead, arcTail, cost) | (arcHead, arcTails) <- M.toList g, (arcTail, cost) <- arcTails]
    where
        f (arcHead, arcTail, cost) = do
            arcTailsOfArcTail <- M.lookup arcTail g
            guard $ (arcHead, cost) `elem` arcTailsOfArcTail
            Just ()

mapCosts :: (Ord a) => (b -> c) -> Graph a b -> Graph a c
mapCosts f (Graph g) = unsafeFromMap $ M.map (map (\(x, y) -> (x, f y))) g

-- | Returns the edge list of a graph. Notably may not contain every vertex in a graph;
-- a vertex may have no edges.
toEdgeList :: Graph a b -> [Edge a b]
toEdgeList (Graph g) = [Edge arcHead arcTail weight | (arcHead, arcTails) <- M.toList g, (arcTail, weight) <- arcTails]

fromList :: forall a b . (Ord a) => [Edge a b] -> Graph a b
fromList xs = mapWithProperVertices
    where
        -- Ensures each vertex appearing in an edge tail appears in the key set of the map.
        mapWithProperVertices = fromList' . M.toList $ baseMap

        baseMap = foldr f M.empty xs

        f :: Edge a b -> M.Map a [(a, b)] -> M.Map a [(a, b)]
        f e acc = M.insertWith (++) e.arcHead [(e.arcTail, e.weight)] acc

edgeCount :: Graph a b -> Int
edgeCount = sum . map length . M.elems . (.m)

-- TODO: Rename to vertexSet
nodes :: Graph a b -> S.Set a
nodes = M.keysSet . (.m)

-- TODO: Rename to vertexList
nodeList :: Graph a b -> [a]
nodeList g = S.toList $ nodes g

vertexCount :: Graph a b -> Int
vertexCount = length . nodeList

flipArcDirections :: (Ord a) => Graph a b -> Graph a b
flipArcDirections g = fromList [Edge x.arcTail x.arcHead x.weight | x <- toEdgeList g]

{- | Adds self loops of the given weight to each node of a graph. -}
addSelfLoops :: (Ord a) => b -> Graph a b -> Graph a b
addSelfLoops b g = merge g selfLoops
    where
        selfLoops = unsafeFromMap $ M.fromList [(node, [(node, b)]) | node <- nodeList g]

hasZeroCostSelfLoops :: (Ord a, Eq b, Num b) => Graph a b -> Bool
hasZeroCostSelfLoops g = all p (nodeList g)
    where
        p arcHead
            | Just arcTails <- M.lookup arcHead g.m, (arcHead, 0) `elem` arcTails = True
            | otherwise = False

-- | Returns a list of all the edges in a that don't have an opposite edge of the same cost.
-- Function performance could easily be improved.
nonSymmetricEdges :: forall a b . (Eq a, Eq b) => Graph a b -> [Edge a b]
nonSymmetricEdges g = filter f edges
    where
        f :: Edge a b -> Bool
        f e1
            | (Just _) <- L.find (\e2 -> isOppositeEdge e1 e2 && e1.weight == e2.weight) edges = False
            | otherwise = True

        edges = toEdgeList g

isOppositeEdge :: (Eq a) => Edge a b -> Edge a b -> Bool
isOppositeEdge e1 e2 = e1.arcTail == e2.arcHead && e1.arcHead == e2.arcTail

-- | Merges the two given graphs.
merge :: (Ord a) => Graph a b -> Graph a b -> Graph a b
merge (Graph g1) (Graph g2) = unsafeFromMap $ M.unionWith (++) g1 g2

merges :: (Foldable f, Ord a) => Graph a b -> f (Graph a b) -> Graph a b
merges initial gs = foldr merge initial gs

merges1 :: (Foldable f, Ord a) => f (Graph a b) -> Graph a b
merges1 gs = foldr1 merge gs

toAlgebraGraph :: Graph a b -> G.Graph a
toAlgebraGraph g = G.overlays [G.edge e.arcHead e.arcTail | e <- toEdgeList g]

toAlgebraGraph' :: Graph a b -> UG.Graph a
toAlgebraGraph' = UG.toUndirected . toAlgebraGraph

-- | Returns a neighbours lookup (a vertex is a neighbour of another vertex if
-- an edge between them exists in either direction). Currently fairly inefficient.
neighbourMap :: forall a b . (Ord a) => Graph a b -> M.Map a (S.Set a)
neighbourMap g = foldr f M.empty $ toEdgeList g
    where
        f :: (Edge a b) -> M.Map a (S.Set a) -> M.Map a (S.Set a)
        f x = M.insertWith (S.union) x.arcTail (S.singleton x.arcHead)
            . M.insertWith (S.union) x.arcHead (S.singleton x.arcTail)

adjacencyList :: (Ord a) => Graph a b -> [(a, S.Set a)]
adjacencyList = M.toList . adjacencyMap

adjacencyMap :: (Ord a) => Graph a b -> M.Map a (S.Set a)
adjacencyMap = M.map (S.fromList . map fst) . (.m)

reverseAdjacencyList :: (Ord a) => Graph a b -> [(a, S.Set a)]
reverseAdjacencyList = adjacencyList . flipArcDirections

reverseAdjacencyMap :: (Ord a) => Graph a b -> M.Map a (S.Set a)
reverseAdjacencyMap = adjacencyMap . flipArcDirections

-- | Returns a subgraph of the given graph that includes only
-- vertices from the given set and those vertices' outgoing edges.
outgoingSubgraph :: (Ord a) => Graph a b -> S.Set a -> Graph a b
outgoingSubgraph (Graph g) new = fromList' $ filter (\(vertex, _) -> S.member vertex new)
                                           $ M.toList g

empty :: (Ord a) => Graph a b
empty = unsafeFromMap M.empty

isEmpty :: Graph a b -> Bool
isEmpty (Graph g) = length g == 0

deleteVertices :: (Ord a) => S.Set a -> Graph a b -> Graph a b
deleteVertices deleted = unsafeFromMap . filterOutOfValues . filterOutOfKeys . (.m)
    where
        filterOutOfKeys = M.filterWithKey (\k _ -> S.notMember k deleted)
        filterOutOfValues = M.map (filter ((`S.notMember` deleted) . fst))

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
induce p g = deleteVertices (S.filter (not . p) (nodes g)) g

minimiseEdgeCosts :: (Ord a, Ord b) => Graph a b -> Graph a b
minimiseEdgeCosts (Graph m) = unsafeFromMap $ M.map (\adj -> U.nubWithBy fst minimise adj) m
    where
        minimise (x1, c1) (x2, c2) = assert (x1 == x2) $ (x1, min c1 c2)
