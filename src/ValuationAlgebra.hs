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


-- Can replace 'nextNodeId' with a function that increments itself like for blockus.
--      A: Before we do this we should probably check it works, and write some tests.
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

