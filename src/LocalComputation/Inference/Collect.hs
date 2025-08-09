{-# LANGUAGE InstanceSigs #-}

module LocalComputation.Inference.Collect
    (
    )
where

-- import           LocalComputation.Inference.JoinTree
-- import           LocalComputation.ValuationAlgebra
--
-- data CollectNode v a b = CollectNode Integer (v a b)
--
-- instance Node CollectNode where
--     collect = undefined
--
--     getValuation (CollectNode _ v) = v
--
--     getDomain (CollectNode _ v) = label v
--
--     create = CollectNode
--
--     nodeId (CollectNode i _) = i
--
-- instance Eq (CollectNode v a b) where
--     x == y = nodeId x == nodeId y
--
-- instance Ord (CollectNode v a b) where
--     x <= y = nodeId x <= nodeId y
--
-- -- Could put implementation inside the Node typeclass and then just call it from in here
-- instance (Valuation v, Show (v a b), Show a, Show b, Ord a, Ord b) => Show (CollectNode v a b) where
--     show (CollectNode i v) = show (i, label v)
--     -- show (CollectNode i d v) = "--------------------"     ++ "\n"
--     --                         ++ "[Index]: "      ++ show i ++ "\n"
--     --                         ++ "[Domain]: "     ++ show d ++ "\n"
--     --                         ++ "[Valuation]: "  ++ show v ++ "\n"
--     --                         ++ "--------------------"     ++ "\n"
--
--
--
