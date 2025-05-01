{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module ValuationAlgebra
    ( Valuation (label, combine, project)
    , Variable, Domain
    , Node
    , collect, getValuation, getDomain, create, nodeId
    , joinTree
    )
where

import Algebra.Graph
import Data.List (intersperse, nub, union)

type Domain a = [a]

setDifference :: (Eq a) => [a] -> [a] -> [a]
setDifference xs ys = filter (\x -> not $ x `elem` ys) xs

domainIntersects :: (Eq a) => Domain a -> Domain a -> Bool
domainIntersects xs ys = or [x == y | x <- xs, y <- ys]

-- When it comes time to make use of the 'possibleValues' field (AKA the 'frame')
-- consider a reimplementation where the 'possibleValues' is instead passed as a
-- mapping from variables to frames to whatever function requires the 'possibleValues'.
--
-- This better reflects the fact that two variables with the same name should have the
-- same frame.
data Variable a b = Variable {
      name :: a
    , possibleValues :: [b]
}

instance (Eq a, Eq b) => Eq (Variable a b) where
    (==) (Variable x1 vs1) (Variable x2 vs2)
        | x1 == x2 && vs1 /= vs2 = error "Two variables with same name had different values"
        | x1 == x2 = True
        | otherwise = False

class Valuation v where
    label   :: v a b -> Domain a
    combine :: v a b -> v a b -> v a b
    project :: v a b -> Domain a -> v a b


-- Note: while the 'Eq', 'Ord', and 'Show' instances can easily be generically written using
-- the exposed parts of the 'Node' typeclass, a typeclass cannot implement another typeclass.
-- Hence, this implementation must be deferred to the implementing type constructors.
class Node n where
    collect         :: (Valuation v) => n v a b -> n v a b
    getValuation    :: (Valuation v) => n v a b -> Maybe (v a b)
    getDomain       :: (Valuation v) => n v a b -> Domain a
    create          :: (Valuation v) => Integer -> Domain a -> Maybe (v a b) -> n v a b
    nodeId          ::                  n v a b -> Integer

-- TODO this should not be specific for BVal - this should be for an valuation that is showable.
-- instance (Eq var, Show var) => Show (CollectNode (BVal varValue) var) where
--     show (CollectNode x y (Just z)) = show x ++ " - " ++ show y ++ " - " ++ (show $ getColumns z)
--     show (CollectNode x y Nothing) = show x ++ " - " ++ show y ++ " - " ++ "Nothing"

-- data ValNode a = ValNode {vertexNum :: Integer, contents :: a} deriving (Show)
-- 
-- instance Eq (ValNode a) where
--     (==) (ValNode {vertexNum = x}) (ValNode {vertexNum = y}) = x == y
-- 
-- instance Ord (ValNode a) where
--     (<=) (ValNode {vertexNum = x}) (ValNode {vertexNum = y}) = x <= y


-- triangulatedGraph :: Graph (ValNode a) -> Graph (ValNode a)
-- triangulatedGraph = undefined
-- 
-- showAdjacents :: (Ord a, Show a) => (Graph a) -> String
-- showAdjacents graph = concat $ intersperse "\n\n\n" $ fmap (show) (adjacencyList graph)
-- 
-- 

-- 1. Can replace 'nextNodeId' with a function that increments itself like for blockus.
--      A: Before we do this we should probably check it works, and write some tests.
--
-- 2. Is this specifically a collect join tree? If so, it should be renamed as such, and instead of
--      taking a generic node it should probably take a collect node. Then i think we go back to
--      typeclass for the generic node, so that we can actually target the collect node as a type.
joinTree :: forall n v a b. (Node n, Valuation v, Eq a, Eq (n v a b))
    => [v a b]
    -> Graph (n v a b)
joinTree vs = edges $ joinTree' nextNodeId r d
    where
        d :: Domain a
        d = foldr union [] $ map label vs

        r :: [n v a b]
        r = zipWith (\nid v -> create nid (label v) (Just v)) [0 ..] vs

        nextNodeId :: Integer
        nextNodeId = fromIntegral $ length r

joinTree' :: forall n v a b. (Node n, Valuation v, Eq a, Eq (n v a b))
    => Integer
    -> [n v a b]
    -> Domain a
    -> [(n v a b, n v a b)]
joinTree' _ _ [] = []
joinTree' nextNodeId r (x : d')
    | length r <= 1 = []
    | length r' > 0 = union (union [(nUnion, nP)] e) (joinTree' (nextNodeId + 2) (union [nP] r') d')
    | otherwise = union e (joinTree' (nextNodeId + 2) r' d')
    where
        xIsInNodeDomain :: n v a b -> Bool
        xIsInNodeDomain n = x `elem` (getDomain n)

        phiX :: [n v a b]
        phiX = filter xIsInNodeDomain r

        domainOfPhiX :: Domain a
        domainOfPhiX = foldr union [] $ map getDomain phiX

        nUnion :: n v a b
        nUnion = create nextNodeId domainOfPhiX Nothing

        r' :: [n v a b]
        r' = setDifference r phiX

        e :: [(n v a b, n v a b)]
        e = [(n, nUnion) | n <- phiX]

        nP :: n v a b
        nP = create (nextNodeId + 1) (setDifference domainOfPhiX [x]) Nothing

{-

Construct the tree by triangulation and then assign numbers and edge directions?
Q: What happens when a node holding P(A | B) points to one holding P(A)?
A: When P(A | B) tries to combine I believe it first marginalizes to P(A)'s domain, so the
     B is removed, and we probably get an identity combination.

-}

-- joinTree :: Graph (ValNode a) -> Graph (ValNode a)
-- joinTree graph = foldr f graph (vertexList graph)
--     where
--         f v acc = undefined

----------------- COLLECT ALGORITHM

-- COLLECT ALGORITHM END
