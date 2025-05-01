{-# LANGUAGE InstanceSigs #-}

module Collect
    (CollectNode
    )
where

import ValuationAlgebra

data CollectNode v a b = CollectNode Integer (Domain a) (Maybe (v a b))

-- instance (Valuation val) => Eq (CollectNode val var) where
--     (==) x y = nodeId x == nodeId y
-- 
-- instance (Valuation val) => Ord (CollectNode val var) where
--     (<=) x y = nodeId x <= nodeId y

instance Node CollectNode where
    collect = undefined

    getValuation (CollectNode _ _ v) = v

    getDomain (CollectNode _ d _) = d

    create = CollectNode

    -- Once we have joinTree working we can move to a version of join tree where we give it a specific node?
    -- type nCreator v a b = Integer -> Domain (Variable a b) -> Maybe (v (Variable a b)) -> n (v (Variable a b))

    nodeId (CollectNode i _ _) = i

instance Eq (CollectNode v a b) where
    x == y = nodeId x == nodeId y

instance Ord (CollectNode v a b) where
    x <= y = nodeId x <= nodeId y

-- Could put implementation inside the Node typeclass and then just call it from in here
instance (Show (v a b), Show a) => Show (CollectNode v a b) where
    show (CollectNode i d v) = "--------------------"     ++ "\n"
                            ++ "[Index]: "      ++ show i ++ "\n"
                            ++ "[Domain]: "     ++ show d ++ "\n"
                            ++ "[Valuation]: "  ++ show v ++ "\n"
                            ++ "--------------------"     ++ "\n"



-- TODO this should not be specific for BVal - this should be for an valuation that is showable.
-- instance (Eq var, Show var) => Show (CollectNode (BVal varValue) var) where
--     show (CollectNode x y (Just z)) = show x ++ " - " ++ show y ++ " - " ++ (show $ getColumns z)
--     show (CollectNode x y Nothing) = show x ++ " - " ++ show y ++ " - " ++ "Nothing"
