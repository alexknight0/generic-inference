{-# LANGUAGE InstanceSigs #-}

module Collect
    (CollectNode
    )
where

import ValuationAlgebra

data CollectNode v a b = CollectNode Integer (Domain a) (Maybe (v a b))

instance Node CollectNode where
    collect = undefined

    getValuation (CollectNode _ _ v) = v

    getDomain (CollectNode _ d _) = d

    create = CollectNode

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

