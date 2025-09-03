{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LocalComputation.Inference.JoinTree
    ( Node (id, v, t)
    , baseJoinTree
    , node
    , changeContent
    , NodeType (Valuation, Query, Union, Projection)
    , redirectToQueryNode
    )
where

import           Algebra.Graph                                  hiding (clique)
import           Data.List                                      (union)
import           Data.Set                                       (fromList,
                                                                 toList)
import qualified Data.Set                                       as S
import qualified LocalComputation.Inference.EliminationSequence as E

import           Data.Maybe                                     (fromJust)
import           GHC.Records                                    (HasField,
                                                                 getField)
import           LocalComputation.ValuationAlgebra

import           Data.Binary                                    (Binary)
import qualified Data.List                                      as L
import           Data.Text.Lazy                                 (unpack)
import           GHC.Generics                                   (Generic)
import           Text.Pretty.Simple                             (pShow)
import           Type.Reflection                                (Typeable)


-- TODO: investigate if union nodes are necessary.

type Id = Integer

data Node v = Node {
      id :: Id
    , v  :: v
    , t  :: NodeType
} deriving (Generic, Typeable, Binary)

data NodeType = Valuation | Query | Union | Projection deriving (Show, Generic, Binary, Enum, Bounded, Eq)

node :: Id -> v -> NodeType -> Node v
node = Node

changeContent :: Node a -> a -> Node a
changeContent n v = n { v = v }

-- | Accessor for the domain of the valuation.  Equivalent to calling `label` on the valuation.
-- __Warning__: Not necessarily O(1).
instance (Valuation v, Ord a, Show a) => HasField "d" (Node (v a)) (Domain a) where
    getField m = label m.v

instance Eq (Node v) where
    x == y = x.id == y.id

instance Ord (Node v) where
    x <= y = x.id <= y.id

instance (Valuation v, Ord a, Show a) => Show (Node (v a)) where
    show n = unpack $ pShow (n.id, n.d)

setDifference :: (Eq a) => [a] -> [a] -> [a]
setDifference xs ys = filter (\x -> not $ x `elem` ys) xs

{- | The core join tree construction algorithm that all required join trees are built from.
This is precisely the join tree construction algorithm described on page 22 of
"A Generic Architecture for Local Computation" by Marc Pouly.

Creates a join tree where every valuation in the original list is assigned to a
leaf node. An effectively random node is assigned the root node. The terminology
'leaf' node and 'root' node may be confusing in this context of directed trees:

                                   5
                          domain {F, L, D, B}

                             ▲    ▲     ▲
                             │    │     │
               ┌─────────────┘    │     └──────┐
               │                ┌─┘            │
               │                │              │
               │                │              │
               │                │              │
                  0            1               2
         domain {F}    domain {L, F}    domain {D, F, B}

In this diagram, nodes 0, 1 and 2 are leaf nodes, and node 5 is the root node.
To make this even more confusing, nodes 0, 1 and 2 are all considered parents of
node 5 (and, accordingly, nodes 0, 1 and 5 consider node 5 their child).

Guarantees a numbering such that nodeId x < nodeId y for any node y that can be
reached from x. The node with the highest id is the root node. Some integers may
be skipped in the assignment of ids to nodes. All nodes either represent valuations,
or are either projection or union nodes. No node was added to represent an 'empty'
domain root.

The query domains are used only to ensure the join tree has a node that answers
each query (by creating empty nodes for each the query domain). The node ids
of the input valuations 'vs' in the final join tree are [0 .. (length vs)] respectively.
-}
baseJoinTree :: forall v a. (Show a, Valuation v, Ord a)
    => [v a]
    -> [Domain a]
    -> Graph (Node (v a))
baseJoinTree vs queries = edges $ baseJoinTree' nextNodeId r d
    where
        d :: E.EliminationSequence a
        d = E.create $ map label vs

        r :: [Node (v a)]
        r =    zipWith (\nid v -> Node nid v            Valuation) [0                        ..] vs
            ++ zipWith (\nid q -> Node nid (identity q) Query)     [fromIntegral (length vs) ..] queries

        nextNodeId :: Id
        nextNodeId = fromIntegral $ length r

{- | For a more general explanation of the overall algorithm see 'baseJoinTree'.

'r' holds the valuations / queries that need to be placed in the join tree in node form.
Each iteration we eliminate a variable, and store all the nodes that had this variable in 'phiX'.
We then add a union node 'nUnion' and add an edge to that union node from each node in 'phiX'.
We then add a projection node 'nP' that has the same domain as 'nUnion' minus the variable we eliminated,
and add an edge from the union node to the projection node.


Where convenient, variables have been named as they appear in the pseudocode described on page 22 of
"A Generic Architecture for Local Computation" by Marc Pouly. Not found in this pseudocode, 'nextNodeId'
is a parameter such that there currently exists no nodes with id > 'nextNodeId' in the tree. As we may
create up to 2 nodes on each iteration, we make the recursive call with 'nextNodeId + 2'
-}
baseJoinTree' :: forall v a . (Valuation v, Ord a, Show a)
    => Id
    -> [Node (v a)]
    -> E.EliminationSequence a
    -> [(Node (v a), Node (v a))]
baseJoinTree' nextNodeId r d
    | E.isEmpty d = []
    | length r <= 1 = []
    | length r' > 0 = union (union [(nUnion, nP)] e) (baseJoinTree' (nextNodeId + 2) (union [nP] r') d')
    | otherwise = union e (baseJoinTree' (nextNodeId + 2) r' d')
    where
        (x, d') = fromJust $ E.eliminateNext d

        xIsInNodeDomain :: Node (v a) -> Bool
        xIsInNodeDomain n = x `elem` (n.d)

        phiX :: [Node (v a)]
        phiX = filter xIsInNodeDomain r

        domainOfPhiX :: Domain a
        domainOfPhiX = foldr (S.union) (S.empty) $ map (.d) phiX

        nUnion :: Node (v a)
        nUnion = Node nextNodeId (identity domainOfPhiX) Union

        r' :: [Node (v a)]
        r' = setDifference r phiX

        e :: [(Node (v a), Node (v a))]
        e = [(n, nUnion) | n <- phiX]

        nP :: Node (v a)
        nP = Node (nextNodeId + 1) (identity nPDomain) Projection
            where
                nPDomain = (fromList $ setDifference (toList domainOfPhiX) [x])

-- TODO: Untested.
-- TODO: Could probably be optimized by caching the adjacencyList computation
-- (but might have to make sure we update it).
{-| Redirects a given **join** tree to reverse edges to face the node of the given id -}
{- Works by traversing out from the given node, flipping any edges that it uses along its journey -}
redirectTree :: forall a . Id -> Graph (Node a) -> Graph (Node a)
redirectTree i g = foldr f g outgoingNodes
    where
        (this, outgoingNodes) = fromJust $ L.find (\(n, _) -> n.id == i) (adjacencyList g)

        f :: Node a -> Graph (Node a) -> Graph (Node a)
        f n acc = flipEdge this n $ redirectTree n.id acc

-- | Redirects the given join tree to reverse edges to face a query node of the given domain.
-- If multiple query nodes with this domain exist, one is chosen at random. This function only
-- searches amongst nodes with a `NodeType` of `Query`.
redirectToQueryNode :: (Valuation v, Ord a, Show a)
    => Domain a -> Graph (Node (v a)) -> Graph (Node (v a))
redirectToQueryNode d g = redirectTree (queryNode.id) g
    where
        queryNode = head $ filter (\n -> n.d == d && n.t == Query) (vertexList g)
--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Flips an edge from x to y such that it now goes from y to x.
flipEdge :: (Eq a) => a -> a -> Graph a -> Graph a
flipEdge x y g
    | hasEdge x y g = addEdge y x $ removeEdge x y $ g
    | otherwise = g

addEdge :: a -> a -> Graph a -> Graph a
addEdge x y g = overlay (connect (vertex x) (vertex y)) g


{- Returns the edge set required to reverse the edges of a **join tree** to face the node of the given id -}
redirectTree' :: Id -> Graph (Node a) -> S.Set (Node a, Node a)
redirectTree' i g = undefined
    where
        outgoingEdges = snd $ fromJust $ L.find (\(n, _) -> n.id == i) (adjacencyList g)



