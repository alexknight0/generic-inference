{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module ValuationAlgebra
    ( Valuation (label, combine, project)
    , Domain
    , Node
    , collect, getValuation, getDomain, create, nodeId
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
    label :: v (Variable a b) -> Domain (Variable a b)
    combine :: v (Variable a b) -> v (Variable a b) -> v (Variable a b)
    project :: v (Variable a b) -> Domain (Variable a b) -> v (Variable a b)

data Node v = CollectNode Integer v

collect :: (Valuation v) => Node (v (Variable a b)) -> Node (v (Variable a b))
collect = undefined

getValuation :: (Valuation v) => Node (v (Variable a b)) -> Maybe (v (Variable a b))
getValuation = undefined

getDomain :: (Valuation v) => Node (v (Variable a b)) -> Domain (Variable a b)
getDomain = undefined

create :: (Valuation v) => Integer -> Domain (Variable a b) -> Maybe (v (Variable a b)) -> Node (v (Variable a b))
create = undefined

nodeId :: (Valuation v) => Node (v (Variable a b)) -> Integer
nodeId = undefined


instance (Valuation v) => Eq (Node (v (Variable a b))) where
    (==) x y = nodeId x == nodeId y

instance (Valuation v) => Ord (Node (v (Variable a b))) where
    (<=) x y = nodeId x <= nodeId y

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
-- showAdjacentsValNode :: Graph (ValNode a) -> String
-- showAdjacentsValNode graph = concat $ intersperse "\n\n" $ fmap showVertices (adjacencyList graph)
--     where
--         showVertices :: (ValNode a, [ValNode a]) -> String
--         showVertices (x, ys) = show (vertexNum x) ++ " -> " ++ (show $ map (\y -> vertexNum y) ys)
-- 
joinTree :: forall v a b. (Valuation v, Eq a, Eq b)
    => [v (Variable a b)]
    -> Graph (Node (v (Variable a b)))
joinTree vs = edges $ joinTree' nextNodeId r d
    where
        d :: Domain (Variable a b)
        d = foldr union [] $ map label vs

        r :: [Node (v (Variable a b))]
        r = zipWith (\nid v -> create nid (label v) (Just v)) [0 ..] vs

        nextNodeId :: Integer
        nextNodeId = fromIntegral $ length r

joinTree' :: forall v a b. (Valuation v, Eq a, Eq b)
    => Integer
    -> [Node (v (Variable a b))]
    -> Domain (Variable a b)
    -> [(Node (v (Variable a b)), Node (v (Variable a b)))]
joinTree' _ _ [] = []
joinTree' nextNodeId r (x : d')
    | length r <= 1 = []
    | length r' > 0 = union (union [(nUnion, nP)] e) (joinTree' (nextNodeId + 2) (union [nP] r') d')
    | otherwise = union e (joinTree' (nextNodeId + 2) r' d')
    where
        xIsInNodeDomain :: Node (v (Variable a b)) -> Bool
        xIsInNodeDomain n = x `elem` (getDomain n)

        phiX :: [Node (v (Variable a b))]
        phiX = filter xIsInNodeDomain r

        domainOfPhiX :: Domain (Variable a b)
        domainOfPhiX = foldr union [] $ map getDomain phiX

        nUnion :: Node (v (Variable a b))
        nUnion = create nextNodeId domainOfPhiX Nothing

        r' :: [Node (v (Variable a b))]
        r' = setDifference r phiX

        e :: [(Node (v (Variable a b)), Node (v (Variable a b)))]
        e = [(n, nUnion) | n <- phiX]

        nP :: Node (v (Variable a b))
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
