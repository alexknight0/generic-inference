{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}

module LocalComputation.ValuationAlgebra
    ( Valuation (label, combine, project, identity, eliminate)
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

-- TODO: document choice of having two types 'a' and 'b'
-- TODO: Could use 'data' constructor inside class declaration...
class Valuation v where
    label     :: (Show a, Show b, Ord a, Ord b) => v a b    -> Domain a
    combine   :: (Show a, Show b, Ord a, Ord b) => v a b    -> v a b    -> v a b
    project   :: (Show a, Show b, Ord a, Ord b) => v a b    -> Domain a -> v a b
    eliminate :: (Show a, Show b, Ord a, Ord b) => v a b    -> Domain a -> v a b
    identity  ::                                   Domain a -> v a b
    -- frame     :: (Ord a, Eq b, Show a) => v a b -> Set (VariableArrangement a b)

    -- What if we force the implementer to implement a data structure for the frame,
    -- where that data structure provides a series of functions that achieves a mapping?
    -- Edit: Well then don't we just have a map?


combines1 :: (Show a, Show b, Foldable f, Valuation v, Ord a, Ord b) => f (v a b) -> v a b
combines1 = foldr1 combine

showDomain :: Domain a -> String
showDomain x = undefined

