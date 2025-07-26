{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}

module LocalComputation.ValuationAlgebra
    ( Valuation (label, combine, project, identity)
    , Domain
    , combines1
    , VariableArrangement
    )
where

import qualified Data.Hashable as H
import qualified Data.Map.Lazy as M
import           Data.Set      (Set)

type Domain a = Set a
type VariableArrangement a b = M.Map a b


class Valuation v where
    label   :: (Show a, Show b, Ord a, Ord b) => v a b -> Domain a
    combine :: (Show a, Show b, Ord a, Ord b) => v a b -> v a b -> v a b
    project :: (Show a, Show b, Ord a, Ord b) => v a b -> Domain a -> v a b
    identity :: Domain a -> v a b

combines1 :: (Show a, Show b, Foldable f, Valuation v, Ord a, Ord b) => f (v a b) -> v a b
combines1 = foldr1 combine

