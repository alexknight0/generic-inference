{-# LANGUAGE InstanceSigs #-}

module Collect
    (
    )
where

import ValuationAlgebra

data CollectNode v a = CollectNode Integer (Domain a) (Maybe v) 

-- instance (Valuation val) => Eq (CollectNode val var) where
--     (==) x y = nodeId x == nodeId y
-- 
-- instance (Valuation val) => Ord (CollectNode val var) where
--     (<=) x y = nodeId x <= nodeId y

instance Node (CollectNode c) where
    collect :: (Valuation v) => CollectNode c (v (Variable a b)) -> CollectNode c (v (Variable a b))
    collect = undefined

    getValuation :: (Valuation v) => n (v (Variable a b)) -> Maybe (v (Variable a b))
    getValuation = undefined

    getDomain :: (Valuation v) => n (v (Variable a b)) -> Domain (Variable a b)
    getDomain = undefined

    -- Once we have joinTree working we can move to a version of join tree where we give it a specific node?
    -- type nCreator v a b = Integer -> Domain (Variable a b) -> Maybe (v (Variable a b)) -> n (v (Variable a b))

    create :: (Valuation v) => Integer -> Domain (Variable a b) -> Maybe (v (Variable a b)) -> n (v (Variable a b))
    create = undefined

    nodeId :: n a -> Integer
    nodeId = undefined

    -- collect = undefined
    -- getValuation (CollectNode _ _ v) = v
    -- getDomain (CollectNode _ d _) = d
    -- create = CollectNode
    -- nodeId (CollectNode i _ _) = i

-- TODO this should not be specific for BVal - this should be for an valuation that is showable.
-- instance (Eq var, Show var) => Show (CollectNode (BVal varValue) var) where
--     show (CollectNode x y (Just z)) = show x ++ " - " ++ show y ++ " - " ++ (show $ getColumns z)
--     show (CollectNode x y Nothing) = show x ++ " - " ++ show y ++ " - " ++ "Nothing"
