{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}

module LocalComputation.ValuationAlgebra
    ( Valuation (label, combine, project, identity, eliminate, VariableArrangement)
    , Domain
    , combines1
    , showDomain
    )
where

import qualified Data.Hashable as H
import qualified Data.List     as L
import qualified Data.Map.Lazy as M
import qualified Data.Set      as S

type Domain a = S.Set a
-- type VariableArrangement a b = M.Map a b

-- TODO: document choice of having two types 'a' and 'b'
-- TODO: Could use 'data' constructor inside class declaration...
class Valuation v where
    type VariableArrangement v a b

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

showDomain :: Show a => Domain a -> String
showDomain x = "{" ++ L.intercalate "," (map show (S.toList x)) ++ "}"

