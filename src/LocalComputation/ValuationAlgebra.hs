{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}

module LocalComputation.ValuationAlgebra
    ( Valuation (label, combine, project, identity, eliminate, VarAssignment)
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
    type VarAssignment v a b

    label     :: (Show a, Ord a) => v a      -> Domain a
    combine   :: (Show a, Ord a) => v a      -> v a      -> v a
    project   :: (Show a, Ord a) => v a      -> Domain a -> v a
    eliminate :: (Show a, Ord a) => v a      -> Domain a -> v a
    identity  ::                    Domain a -> v a
    -- frame     :: (Ord a, Eq b, Show a) => v a b -> Set (VariableArrangement a b)

    -- What if we force the implementer to implement a data structure for the frame,
    -- where that data structure provides a series of functions that achieves a mapping?
    -- Edit: Well then don't we just have a map?


combines1 :: (Show a, Foldable f, Valuation v, Ord a) => f (v a) -> v a
combines1 = foldr1 combine

showDomain :: Show a => Domain a -> String
showDomain x = "{" ++ L.intercalate "," (map show (S.toList x)) ++ "}"

