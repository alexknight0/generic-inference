{-# LANGUAGE ScopedTypeVariables #-}

module JoinTree
    ( Node
    , collect, getValuation, getDomain, create, nodeId
    , baseJoinTree
    )
where

import           Algebra.Graph    hiding (clique)
import qualified Data.Heap        as H
import           Data.List        (union)
import qualified Data.Map         as M
import           Data.Set         (fromList, toList)
import qualified Data.Set         (empty, union)
import qualified Data.Set         as S

import           ValuationAlgebra

class Node n where
    collect         :: (Valuation v) => n v a b -> n v a b
    getValuation    :: (Valuation v) => n v a b -> v a b
    getDomain       :: (Valuation v) => n v a b -> Domain a
    create          :: (Valuation v) => Integer -> Domain a -> v a b -> n v a b
    nodeId          ::                  n v a b -> Integer

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
baseJoinTree :: forall n v a b. (Show a, Show b, Node n, Valuation v, Ord a, Ord b, Eq (n v a b))
    => [v a b]
    -> [Domain a]
    -> Graph (n v a b)
baseJoinTree vs queries = edges $ baseJoinTree' nextNodeId r d
    where
        d :: [a]
        d = foldr (union . toList) [] $ map label vs

        r :: [n v a b]
        r = zipWith (\nid v -> create nid (label v) v) [0 ..] vs
            ++ zipWith (\nid q -> create nid q (identity q)) [fromIntegral (length vs) ..] queries

        nextNodeId :: Integer
        nextNodeId = fromIntegral $ length r

{- | For an explanation of the overall algorithm see 'baseJoinTree'

Where convenient, variables have been named as they appear in the pseudocode described on page 22 of
"A Generic Architecture for Local Computation" by Marc Pouly. Not found in this pseudocode, 'nextNodeId'
is a parameter such that there currently exists no nodes with id > 'nextNodeId' in the tree. As we may
create up to 2 nodes on each iteration, we make the recursive call with 'nextNodeId + 2'
-}
baseJoinTree' :: forall n v a b. (Node n, Valuation v, Eq (n v a b), Ord a)
    => Integer
    -> [n v a b]
    -> [a]
    -> [(n v a b, n v a b)]
baseJoinTree' _ _ [] = []
baseJoinTree' nextNodeId r (x : d')
    | length r <= 1 = []
    | length r' > 0 = union (union [(nUnion, nP)] e) (baseJoinTree' (nextNodeId + 2) (union [nP] r') d')
    | otherwise = union e (baseJoinTree' (nextNodeId + 2) r' d')
    where
        xIsInNodeDomain :: n v a b -> Bool
        xIsInNodeDomain n = x `elem` (getDomain n)

        phiX :: [n v a b]
        phiX = filter xIsInNodeDomain r

        domainOfPhiX :: Domain a
        domainOfPhiX = foldr (S.union) (S.empty) $ map getDomain phiX

        nUnion :: n v a b
        nUnion = create nextNodeId domainOfPhiX (identity domainOfPhiX)

        r' :: [n v a b]
        r' = setDifference r phiX

        e :: [(n v a b, n v a b)]
        e = [(n, nUnion) | n <- phiX]

        nP :: n v a b
        nP = create (nextNodeId + 1) nPDomain (identity nPDomain)
            where
                nPDomain = (fromList $ setDifference (toList domainOfPhiX) [x])

