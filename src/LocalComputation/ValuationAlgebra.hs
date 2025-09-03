{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuantifiedConstraints  #-}
{-# LANGUAGE TypeFamilies           #-}

module LocalComputation.ValuationAlgebra
    ( Valuation (label, combine, combine_, project, project_, identity, eliminate, satisfiesInvariants, VarAssignment)
    , Domain
    , Var
    , combines1
    , showDomain
    )
where

import           Control.Exception      (assert)
import qualified Data.Hashable          as H
import qualified Data.List              as L
import qualified Data.Map.Lazy          as M
import qualified Data.Set               as S
import           GHC.Base               (Constraint)
import qualified LocalComputation.Utils as U

type Domain a = S.Set a

type Var a = (Show a, Ord a)

-- TODO: document choice of having two types 'a' and 'b'
-- TODO: Could use 'data' constructor inside class declaration...
class Valuation v where

    type VarAssignment v a b

    label      :: Var a => v a -> Domain a

    combine    :: Var a => v a -> v a      -> v a
    combine v1 v2 = U.assertP satisfiesInvariants $ combine_ v1 v2

    combine_   :: Var a => v a -> v a      -> v a

    project    :: Var a => v a -> Domain a -> v a
    project v d
        -- Domain projected to must be subset.
        | assert (d `S.isSubsetOf` label v) False = undefined
        -- If current domain is domain of projection skip projection call for efficency.
        | label v == d = v
        -- Delegate call to project_ but check invariants on return
        | otherwise = U.assertP satisfiesInvariants $ project_ v d
    project_   :: Var a => v a -> Domain a -> v a

    -- TODO: Default implementation of eliminate
    eliminate  :: Var a => v a -> Domain a -> v a

    identity  :: Domain a -> v a

    satisfiesInvariants :: Var a => v a -> Bool
    satisfiesInvariants _ = True


combines1 :: (Foldable f, Valuation v, Var a) => f (v a) -> v a
combines1 = foldr1 combine

showDomain :: Show a => Domain a -> String
showDomain x = "{" ++ L.intercalate "," (map show (S.toList x)) ++ "}"

{- TODO: Add to future work / methodology / results:

The Valuation typeclass has been written supposing the only property that an implementer
requires of the variables for a given valuation algebra is that they can be ordered.

\footnote{Technically we also require a \codeterm{Show} instance (i.e. the ability to turn an
          arbitrary variable into a \codeterm{String}). However this is only for the purpose
          of visual output, and does not in any way impact the computation of inference. Hence,
          it is not a constraint on the types of variables we can define valuation algebras over
          (for example, we could trivially define a \codeterm{Show} instance that converts
          every element to the empty \codeterm{String} and this would suffice)
}

The requirement for an ordering is more strict than necessary, simply coming from the fact
that the data structure chosen to represent the 'Domain' is a \codeterm{Set} that has been
implemented using balanced binary trees and hence requires some way to order the variables
\cite{https://hackage-content.haskell.org/package/containers-0.8/docs/Data-Set.html}.
However, this property still proves useful in many valuation algebra instances as a way to
improve performance.

Ideally, there would be no constraints on the variables at the top level, and each valuation
algebra instance would be allowed to define it's own constraints for the variables. This
can be achieved by moving removing the `type Var a` and `type Domain a` declarations into
the `Valuation` typeclass. This will allow the implementer of a `Valuation` instance to
specify what constraints are required on the variables of their valuation algebra and
how domains of these variables are represented. Implementation wise, this change will require
changing `Var` and `Domain` into type families, and the addition of extra functions to the
`Valuation` typeclass that achieve the operations on domains that are required by the generic
inference algorithm.

\footnote{Indeed, forcing the implementation of a `Domain` type and the additional functions to
          operate on the `Domain` type would impose a larger burden on the implementer of an
          instance of the `Valuation` typeclass. However, allowing the type of `Domain` to
          default to a known set implementation and then defaulting the required methods such that
          they operate on this set implementation may provide a way to avoid this burden
}

This refactor would allow further freedom for implementers of a `Valuation` instance to
impose any constraints on the variables of their instance that they desire, potentially allowing
otherwise impossible instances of a valuation algebra or performance improvements. For example,
constraining the variables of a given instance to variables that implement the `Hashable` typeclass
could allow the use of hashtables which could provide better performance than alternatives
in certain circumstances.

// The requirement for an ordering is more strict than necessary; we could get by
// with only equality, as strictly the only thing we require is the inescapable ability
// to tell two different variables apart.
//
// ^^^^^^^^^^^^
//  TODO: While this passage is true, it seems irrelevant to argue that we believe
//  that equality is a necessary constraint in all useful instances of a valuation algebra
//  as it actually isn't necessary for discussion - all we say here is that we hope for
//  an implementation that makes no assumptions and outline a promising method of getting there.

-}
