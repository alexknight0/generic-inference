{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}

module ValuationAlgebra
    ( Valuation (label, combine, project)
    , Domain
    , Node
    , collect, getValuation, getDomain, create, nodeId
    )
where

type Domain a = [a]

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


