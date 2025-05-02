{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module JoinTree
    ( Node
    , collect, getValuation, getDomain, create, nodeId
    , baseJoinTree
    )
where

import Algebra.Graph
import Data.List (union)

import ValuationAlgebra

-- Note: while the 'Eq', 'Ord', and 'Show' instances can easily be generically written using
-- the exposed parts of the 'Node' typeclass, a typeclass cannot implement another typeclass.
-- Hence, this implementation must be deferred to the implementing type constructors.
class Node n where
    collect         :: (Valuation v) => n v a b -> n v a b
    getValuation    :: (Valuation v) => n v a b -> v a b
    getDomain       :: (Valuation v) => n v a b -> Domain a
    create          :: (Valuation v) => Integer -> Domain a -> v a b -> n v a b
    nodeId          ::                  n v a b -> Integer

setDifference :: (Eq a) => [a] -> [a] -> [a]
setDifference xs ys = filter (\x -> not $ x `elem` ys) xs

domainIntersects :: (Eq a) => Domain a -> Domain a -> Bool
domainIntersects xs ys = or [x == y | x <- xs, y <- ys]

-- Can replace 'nextNodeId' with a function that increments itself like for blockus.
--      A: Before we do this we should probably check it works, and write some tests.
-- The core join tree construction algorithm that all required join trees are built from.
-- This is precisely the join tree construction algorithm described on page 22 of
-- "A Generic Architecture for Local Computation" by Marc Pouly.
--
-- Creates a join tree where every valuation in the original list is assigned to a
-- leaf node. An effectively random node is assigned the root node. The terminology
-- 'leaf' node and 'root' node may be confusing in this context of directed trees:
--
--                                    5
--                           domain {F, L, D, B}
--
--                              ▲    ▲     ▲
--                              │    │     │
--                ┌─────────────┘    │     └──────┐
--                │                ┌─┘            │
--                │                │              │
--                │                │              │
--                │                │              │
--                   0            1               2
--          domain {F}    domain {L, F}    domain {D, F, B}
--
-- In this diagram, nodes 0, 1 and 2 are leaf nodes, and node 5 is the root node.
-- To make this even more confusing, nodes 0, 1 and 2 are all considered parents of
-- node 5 (and, accordingly, nodes 0, 1 and 5 consider node 5 their child).
--
-- Guarantees a numbering such that nodeId x < nodeId y for any node y that can be
-- reached from x. The node with the highest id is the root node. Some integers may
-- be skipped in the assignment of ids to nodes. All nodes either represent valuations,
-- or are either projection or union nodes. No node was added to represent an 'empty'
-- domain root.
baseJoinTree :: forall n v a b. (Node n, Valuation v, Eq a, Eq (n v a b))
    => [v a b]
    -> Graph (n v a b)
baseJoinTree vs = edges $ baseJoinTree' nextNodeId r d
    where
        d :: Domain a
        d = foldr union [] $ map label vs

        r :: [n v a b]
        r = zipWith (\nid v -> create nid (label v) v) [0 ..] vs

        nextNodeId :: Integer
        nextNodeId = fromIntegral $ length r

baseJoinTree' :: forall n v a b. (Node n, Valuation v, Eq a, Eq (n v a b))
    => Integer
    -> [n v a b]
    -> Domain a
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
        domainOfPhiX = foldr union [] $ map getDomain phiX

        nUnion :: n v a b
        nUnion = create nextNodeId domainOfPhiX identity

        r' :: [n v a b]
        r' = setDifference r phiX

        e :: [(n v a b, n v a b)]
        e = [(n, nUnion) | n <- phiX]

        nP :: n v a b
        nP = create (nextNodeId + 1) (setDifference domainOfPhiX [x]) identity


