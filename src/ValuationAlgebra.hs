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
    label   :: (Show a, Show b, Ord a, Ord b) => v a b -> Domain a
    combine :: (Show a, Show b, Ord a, Ord b) => v a b -> v a b -> v a b
    project :: (Show a, Show b, Ord a, Ord b) => v a b -> Domain a -> v a b
    identity :: Domain a -> v a b

combines :: (Show a, Show b, Foldable f, Valuation v, Ord a, Ord b) => f (v a b) -> v a b
combines = foldr1 combine


