{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}

module ValuationAlgebra
    ( Valuation (label, combine, project, identity)
    , Domain
    , combines
    )
where

import           Data.Set (Set)

type Domain a = Set a

class Valuation v where
    label   :: (Ord a) => v a b -> Domain a
    combine :: (Ord a, Ord b) => v a b -> v a b -> v a b
    project :: (Ord a, Ord b) => v a b -> Domain a -> v a b
    identity :: v a b

combines :: (Foldable f, Valuation v, Ord a, Ord b) => f (v a b) -> v a b
combines = foldr combine identity


