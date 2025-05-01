{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}

module ValuationAlgebra
    ( Valuation (label, combine, project)
    , Domain
    )
where

type Domain a = [a]

class Valuation v where
    label   :: v a b -> Domain a
    combine :: v a b -> v a b -> v a b
    project :: v a b -> Domain a -> v a b


