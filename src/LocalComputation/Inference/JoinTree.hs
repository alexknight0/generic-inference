{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LocalComputation.Inference.JoinTree (

    -- Join tree construction
      baseJoinForest
    , collectTree
    , binaryJoinForest

    -- Join tree type
    , JoinTree
    , JoinForest
    , Node (id, v, t, postbox)
    , NodeType (Valuation, Query, Union, Projection)
    , node
    , changeContent
) where

import           Data.List                                      (union)
import           Data.Set                                       (fromList,
                                                                 toList)
import qualified Data.Set                                       as S
import qualified LocalComputation.Inference.EliminationSequence as E
import           LocalComputation.Inference.JoinTree.Forest
import           LocalComputation.Inference.JoinTree.Tree       (Id, JoinTree,
                                                                 Node (..),
                                                                 NodeType (..),
                                                                 changeContent,
                                                                 node)

import           Data.Maybe                                     (fromJust)
import           LocalComputation.ValuationAlgebra

import qualified Algebra.Graph.AdjacencyMap                     as G
import           Control.Exception                              (assert)
import           Extra                                          (minimumOn)
import qualified LocalComputation.Utils                         as U

--------------------------------------------------------------------------------
-- Join tree creation algorithms
--------------------------------------------------------------------------------
-- Both join tree creation algorithms are built such that projections that occur during
-- a collect phase at most remove one variable (i.e. they are equivalent to variable eliminations).
-- When the distribute phase starts, projections may occur that remove more than one variable as
-- union nodes will feed information back to 'parents', which may include valuations and queries
-- which may have much smaller domains.
--
-- Notably if the join tree is redirected, as in `collectTree`, we may get some projections that
-- remove more than one variable during the collect phase. For example, the query node will have
-- been attached to a union node during construction, and this union node may have a much larger
-- domain than the query node. When tree is redirected, we now have a large projection on the message
-- sent to the query node as it is received. However, there should not be too many such projections.

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
baseJoinForest :: forall v a. (Show a, ValuationFamily v, Ord a)
    => [v a]
    -> [Domain a]
    -> JoinForest (v a)
baseJoinForest vs queries = unsafeFromGraph $ G.edges $ baseJoinForest' nextNodeId r d
    where
        d :: E.EliminationSequence a
        d = E.create $ map label vs

        r :: [Node (v a)]
        r =    zipWith (\nid v -> node nid v            Valuation) [0                        ..] vs
            ++ zipWith (\nid q -> node nid (identity q) Query)     [fromIntegral (length vs) ..] queries

        nextNodeId :: Id
        nextNodeId = fromIntegral $ length r

{- | For a more general explanation of the overall algorithm see 'baseJoinForest'.

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
baseJoinForest' :: forall v a . (ValuationFamily v, Ord a, Show a)
    => Id
    -> [Node (v a)]
    -> E.EliminationSequence a
    -> [(Node (v a), Node (v a))]
baseJoinForest' nextNodeId r d
    | E.isEmpty d = []
    | length r <= 1 = []
    | length r' > 0 = union (union [(nUnion, nP)] e) (baseJoinForest' (nextNodeId + 2) (union [nP] r') d')
    | otherwise = union e (baseJoinForest' (nextNodeId + 2) r' d')
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

binaryJoinForest :: forall v a. (Show a, ValuationFamily v, Ord a)
    => [v a]
    -> [Domain a]
    -> JoinForest (v a)
binaryJoinForest vs queries = unsafeFromGraph $ G.overlay (G.vertices newPhiU) (G.edges edges)

    where
        initialPsiU = E.create $ map label vs

        initialPhiU :: [Node (v a)]
        initialPhiU =    zipWith (\nid v -> node nid v            Valuation) [0                        ..] vs
                      ++ zipWith (\nid q -> node nid (identity q) Query)     [fromIntegral (length vs) ..] queries

        initialK :: Id
        initialK = fromIntegral $ length initialPhiU

        initialEdges = []

        (newPhiU, edges) = outerLoop initialPsiU initialPhiU initialEdges initialK

binaryJoinForest' :: forall v a. (Show a, ValuationFamily v, Ord a)
    => E.EliminationSequence a
    -> [Node (v a)]
    -> [(Node (v a), Node (v a))]
    -> [(Node (v a), Node (v a))]
binaryJoinForest' = undefined

outerLoop :: forall v a. (Show a, ValuationFamily v, Ord a)
    => E.EliminationSequence a
    -> [Node (v a)]
    -> [(Node (v a), Node (v a))]
    -> Id
    -> ([Node (v a)], [(Node (v a), Node (v a))])
outerLoop psiU phiU edges k
    | length phiU <= 1 = (phiU, edges)
    | E.size psiU > 0  = let sKAfterLoop = node kAfterLoop (identity $ S.difference r.d (S.singleton y)) Projection
                             newK = kAfterLoop + 1
                             newEdges = (r, sKAfterLoop) : edgesAfterLoop
                             newPhiU = (sKAfterLoop : phiU) `setDifference` filter yIsInNodeDomain phiU
                             newPsiU = psiUMinusY

                             invariant = not $ any (\x -> S.member y x.d) newPhiU
                         in assert invariant $ outerLoop newPsiU newPhiU newEdges newK
    | otherwise        = (phiU, edges)


                        -- = let newEdges = edgesAfterLoop
                        --       newK = kAfterLoop
                        --       newPhiU = phiU -- `setDifference` filter yIsInNodeDomain phiU
                        --       newPsiU = psiU
                        --   in trace (show $ newPhiU) $ outerLoop newPsiU newPhiU newEdges newK

    where
        (y, psiUMinusY) = fromJust $ E.eliminateNext psiU

        yIsInNodeDomain :: Node (v a) -> Bool
        yIsInNodeDomain n = y `elem` (n.d)

        phiY = filter yIsInNodeDomain phiU

        -- Parameters after inner loop
        (r, edgesAfterLoop, kAfterLoop) = innerLoop phiY edges k

-- | The inner loop of the pseudocode for binary join forest construction (see `binaryJoinForest`)
--
-- `k` represents the next id that should be used to create a new node with an unused id.
-- `phiY` is the set of nodes (query nodes / valuation nodes that have to be added to the join tree)
-- that contain the variable 'y' (the variable 'y' is not used in this function).
-- `edges` is the set of edges of the final graph
--
-- Observe that each loop the length of phiY decreases by exactly one!
innerLoop :: forall v a. (Valuation v a)
    => [Node (v a)]
    -> [(Node (v a), Node (v a))]
    -> Id
    -> (Node (v a), [(Node (v a), Node (v a))], Id)
innerLoop phiY edges k
    | length phiY <= 1 = (head phiY, edges, k)
    | assert invariant False = undefined
    | otherwise = innerLoop newPhiY newEdges newK
    where
        -- TODO: Fix
        (r1, r2) = foldr1 (\(x1, x2) (y1, y2) -> if x1.d == S.empty && x2.d == S.empty then (x1, x2) else
                                                    if length (S.union x1.d x2.d) < length (S.union y1.d y2.d)
                                                            then (x1, x2) else (y1, y2))

                            $ map U.toTuple $ U.combinations 2 phiY

        sK :: Node (v a)
        sK = node k (identity $ S.union r1.d r2.d) Union

        newEdges = (r1, sK) : (r2, sK) : edges
        newPhiY = sK : phiY `setDifference` [r1, r2]
        newK = k + 1

        -- Each loop the length of phiY decreases by exactly one
        invariant = length newPhiY == length phiY - 1


-- | Creates a join tree that can be used for collect problems.
collectTree :: (Show a, ValuationFamily v, Ord a)
    => [v a]
    -> Domain a
    -> JoinTree (v a)
-- TODO: UPDATE UPDATE UPDATE to binary join tree i think
collectTree vs q = unsafeConvertToCollectTree (binaryJoinForest vs [q]) q
                -- ^^^ call is safe in this case.

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

setDifference :: (Eq a) => [a] -> [a] -> [a]
setDifference xs ys = filter (\x -> not $ x `elem` ys) xs
