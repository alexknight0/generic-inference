{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LocalComputation.Inference.JoinTree (

    -- Join tree construction
      baseJoinTree

    -- Join tree type
    , JoinTree
    , Node (id, v, t)
    , NodeType (Valuation, Query, Union, Projection)
    , node
    , changeContent

    -- Join tree functions
    , redirectToQueryNode
    , unsafeFindById
    , unsafeOutgoingEdges
    , unsafeIncomingEdges
    , unsafeOutgoingEdges'
    , unsafeIncomingEdges'
    , neighbourMap
    , vertexList
    , mapVertices
    , flipEdge
    , topologicalOrdering
    , isForest
    , supportsCollect
) where

import           Data.List                                      (union)
import           Data.Set                                       (fromList,
                                                                 toList)
import qualified Data.Set                                       as S
import qualified LocalComputation.Inference.EliminationSequence as E
import           LocalComputation.Inference.JoinTree.Tree

import           Data.Maybe                                     (fromJust)
import           LocalComputation.ValuationAlgebra

import qualified Algebra.Graph                                  as G
import qualified Data.Map                                       as M
import qualified LocalComputation.Utils                         as U

--------------------------------------------------------------------------------
-- Join tree creation algorithms
--------------------------------------------------------------------------------

-- TODO: A seperate type for join trees that guarantees properties such as the root
-- node having the largest label (a property currently used by fusion message passing)
-- should be utilised.
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
    -> JoinTree (v a)
baseJoinTree vs queries = fromGraph $ G.edges $ baseJoinTree' nextNodeId r d
    where
        d :: E.EliminationSequence a
        d = E.create $ map label vs

        r :: [Node (v a)]
        r =    zipWith (\nid v -> node nid v            Valuation) [0                        ..] vs
            ++ zipWith (\nid q -> node nid (identity q) Query)     [fromIntegral (length vs) ..] queries

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
        nUnion = node nextNodeId (identity domainOfPhiX) Union

        r' :: [Node (v a)]
        r' = setDifference r phiX

        e :: [(Node (v a), Node (v a))]
        e = [(n, nUnion) | n <- phiX]

        nP :: Node (v a)
        nP = node (nextNodeId + 1) (identity nPDomain) Projection
            where
                nPDomain = (fromList $ setDifference (toList domainOfPhiX) [x])

collectTree :: (Show a, Valuation v, Ord a)
    => [v a]
    -> Domain a
    -> JoinTree (v a)
collectTree vs q = U.assertP supportsCollect $ redirectToQueryNode q $ baseJoinTree vs [q]

--------------------------------------------------------------------------------
-- Join tree algorithms
--------------------------------------------------------------------------------

-- TODO: Untested.
-- TODO: Could probably be optimized by caching the adjacencyList computation
-- (but might have to make sure we update it).
{-| Redirects a given join tree to reverse edges to face the node of the given id.
If a forest is given, will not impact trees that don't contain the node of the given id.
-}
{- Works by traversing out from the given node, flipping any edges that it uses along its journey -}
redirectTree :: forall a . Id -> JoinTree a -> JoinTree a
redirectTree i g = foldr f g outgoingNodes
    where
        (this, outgoingNodes) = unsafeOutgoingEdges' i g

        f :: Node a -> JoinTree a -> JoinTree a
        f n acc = flipEdge this n $ redirectTree n.id acc

renumberTree :: JoinTree a -> JoinTree a
renumberTree g = mapVertices (\n -> changeId n $ (M.!) newNumbering n.id) g
    where
        -- TODO: Fix.
        topological = U.assertP (\l -> (last l).t == Query) $ topologicalOrdering g
        newNumbering = M.fromList $ zip (map (.id) topological) [0..]

        changeId n newId = n { id = newId }

-- | Redirects the given join tree to reverse edges to face a query node of the given domain.
-- If multiple query nodes with this domain exist, one is chosen at random. This function only
-- searches amongst nodes with a `NodeType` of `Query`.
redirectToQueryNode :: (Valuation v, Ord a, Show a)
    => Domain a -> JoinTree (v a) -> JoinTree (v a)
redirectToQueryNode d g = renumberTree $ redirectTree (queryNode.id) g
    where
        queryNode = head $ filter (\n -> n.d == d && n.t == Query) (vertexList g)

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

setDifference :: (Eq a) => [a] -> [a] -> [a]
setDifference xs ys = filter (\x -> not $ x `elem` ys) xs
