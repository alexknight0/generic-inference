{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ShenoyShafer
    (
    )
where

import qualified Algebra.Graph
import Algebra.Graph.Undirected

import ValuationAlgebra
import JoinTree


data ShenoyShaferNode v a b = ShenoyShaferNode Integer (Domain a) (v a b)

instance Node ShenoyShaferNode where
    collect = undefined

    getValuation (ShenoyShaferNode _ _ v) = v

    getDomain (ShenoyShaferNode _ d _) = d

    create = ShenoyShaferNode

    nodeId (ShenoyShaferNode i _ _) = i

instance Eq (ShenoyShaferNode v a b) where
    x == y = nodeId x == nodeId y

instance Ord (ShenoyShaferNode v a b) where
    x <= y = nodeId x <= nodeId y

-- Could put implementation inside the Node typeclass and then just call it from in here
instance (Show (v a b), Show a) => Show (ShenoyShaferNode v a b) where
    show (ShenoyShaferNode i d v) = "--------------------"     ++ "\n"
                            ++ "[Index]: "      ++ show i ++ "\n"
                            ++ "[Domain]: "     ++ show d ++ "\n"
                            ++ "[Valuation]: "  ++ show v ++ "\n"
                            ++ "--------------------"     ++ "\n"



joinTree :: forall n v a b. (Node n, Valuation v, Eq a, Eq (n v a b))
    => [v a b]
    -> Graph (n v a b)
joinTree vs = toUndirected (baseJoinTree vs)


