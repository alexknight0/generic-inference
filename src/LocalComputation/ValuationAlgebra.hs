{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE CPP                    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuantifiedConstraints  #-}
{-# LANGUAGE TypeFamilies           #-}

{- | If an unsafe library function says it *asserts* something is the case,
that literally means it uses the `Control.Exception` `assert`. Hence, the user
should not rely on an error being thrown unless asserts are enabled.

TODO: Move to head of library.
-}
module LocalComputation.ValuationAlgebra
    ( ValuationFamily (label, _combine, _project, identity, eliminate, satisfiesInvariants, VarAssignment, combineAssignments, projectAssignment, configurationExtSet, emptyAssignment, frameLength, notIdentity)
    , labelSize
    , combine
    , project
    , combines1
    , Domain
    , Var
    , showDomain
    , assertInvariants
    , isIdentity
    , Valuation
    , combineCounter
    , projectCounter
    , IntOrInfinity (..)

    -- Serialization related typeclasses
    , Binary   -- Must be derived for serialization
    , Typeable -- Must be derived for serialization
    , Generic  -- Must be derived to derive Binary
    , NFData   -- Must be derived for to allow returning as a result of 'Process'.
    , Serializable
    )
where

import           Control.Exception                        (assert)
import qualified Data.Hashable                            as H
import qualified Data.List                                as L
import qualified Data.Map.Lazy                            as M
import qualified Data.Set                                 as S
import           GHC.Base                                 (Constraint)
import qualified LocalComputation.Utils                   as U

import           Control.DeepSeq                          (NFData)
import           Control.Distributed.Process.Serializable (Serializable)
import           Data.Binary                              (Binary)
import qualified Data.IORef                               as IO
import           Data.Proxy                               (Proxy)
import           GHC.Generics                             (Generic)
import qualified System.IO.Unsafe                         as IO
import           Type.Reflection                          (Typeable)

type Domain a = S.Set a

type Var a = (Show a, Ord a)

type Valuation v a = (ValuationFamily v, Var a)

-- TODO: Add to future work
-- page 132 generic inference - shenoy has worse speed but better memory use,
-- should consider other methods...

-- TODO: Do we want to undo the _fusion _combine change?
-- Answer: No, because of the assumption we check on 'project'
-- TODO: If the above is the case, then we should have the same
-- for eliminate as it also has an assumption case; and we don't
-- want that overridden if eliminate is overridden.
-- TODO: Is there any way we can get automatic property checking here?
-- Answer: Probably yes, but it would require an implementation of a Gen
-- for the given type, which can be hard its self.

-- TODO: document choice of having two types 'a' and 'b'
-- TODO: Could use 'data' constructor inside class declaration...

data IntOrInfinity = Int Int | Infinity deriving (Eq, Ord, NFData, Generic, Show)

-- | A valuation belonging to a certain family of valuation algebras
--
-- The identity function should return a valuation that behaves like
-- an identity element under combination. Additionally, differing from
-- Marc Pouly's use of an identity element in "Generic Inference" (page 91),
-- we give the identity element a domain on creation and expect this domain
-- to be upheld like any other domain under valuation algebra operations.
-- This is done to simplify the implementation of join tree construction
-- and message passing - with this implementation, the domain of a node in
-- a join tree is always the same as the label of the valuation (identity
-- or otherwise) that has been assigned to that node. This can be changed,
-- but asserts should then be updated to no longer enforce that projections
-- must occur to subsets of domains when occuring between an identity element
-- and a valuation.
class ValuationFamily v where

    label     :: Var a => v a -> Domain a
    _combine  :: Var a => v a -> v a      -> v a
    _project  :: Var a => v a -> Domain a -> v a

    eliminate :: Var a => v a -> Domain a -> v a
    eliminate v d = project v (S.difference (label v) d)

    frameLength :: Var a => a -> v a -> IntOrInfinity

    identity  :: Domain a -> v a
    isIdentity :: v a -> Bool
    notIdentity :: v a -> Bool
    notIdentity = not . isIdentity

    -- TODO: Remove.
    satisfiesInvariants :: Var a => v a -> Bool
    satisfiesInvariants _ = True

    type VarAssignment v a
    combineAssignments :: Var a => Proxy (v a) -> VarAssignment v a -> VarAssignment v a -> VarAssignment v a
    projectAssignment  :: Var a => Proxy (v a) -> VarAssignment v a -> Domain a          -> VarAssignment v a
    emptyAssignment    :: Var a => Proxy (v a) -> VarAssignment v a

    -- | Produces the configuration extension set.
    --
    -- Given a variable assignment 'x', and a valuation of domain 's', we may want a
    -- variable assignment that adds additional assignments to 'x' to form a larger
    -- variable assignment with domain 's'. A configuration extension set is the set
    -- of valid variable assignments we could add to 'x' to achieve this.
    --
    -- See page 368 of Marc Pouly's "Generic Inference" for more details.
    configurationExtSet :: Var a => v a -> VarAssignment v a -> S.Set (VarAssignment v a)


combine :: (ValuationFamily v, Var a) => v a -> v a -> v a
#if !defined(COUNT_OPERATIONS) || !(COUNT_OPERATIONS)
combine v1 v2 = assertInvariants $ _combine v1 v2
#else
{-# NOINLINE combine #-}
combine v1 v2 = IO.unsafePerformIO $ do
    -- Increment if not a trivial combination.
    case notIdentity v1 && notIdentity v2 of
        True -> U.incrementGlobal combineCounter >> pure ()
        _    -> pure ()
    pure $ assertInvariants $ _combine v1 v2
#endif

project :: (ValuationFamily v, Var a) => v a -> Domain a -> v a
#if !defined(COUNT_OPERATIONS) || !(COUNT_OPERATIONS)
project v d
    -- If current domain is subset of domain of projection skip projection call for efficency.
    | S.isSubsetOf (label v) d = v
    -- Delegate call to _project but check invariants on return
    | otherwise = assertInvariants $ _project v d
#else
{-# NOINLINE project #-}
project v d
    -- If current domain is subset of domain of projection skip projection call for efficency.
    | S.isSubsetOf (label v) d = v
    -- Delegate call to _project but check invariants on return
    | otherwise = IO.unsafePerformIO $ do
        -- Increment if not a trivial projection
        case notIdentity v of
            True -> U.incrementGlobal projectCounter >> pure ()
            _    -> pure ()
        pure $ assertInvariants $ _project v d
#endif

assertInvariants :: (ValuationFamily v, Var a) => v a -> v a
assertInvariants v = U.assertP satisfiesInvariants v

labelSize :: Valuation v a => v a -> Int
labelSize = length . label

combines1 :: (Foldable f, ValuationFamily v, Var a) => f (v a) -> v a
combines1 = foldr1 combine

showDomain :: Show a => Domain a -> String
showDomain x = "{" ++ L.intercalate "," (map show (S.toList x)) ++ "}"

-- TODO: Fix

-- maxFrameLength :: Valuation v a => v a -> [v a] -> IntOrInfinity
-- maxFrameLength valuation valuations = maximum . map (\var -> frameLength var valuation) . S.toList . label $ valuation

--------------------------------------------------------------------------------
-- Counting Operations
--------------------------------------------------------------------------------
{-# NOINLINE combineCounter #-}
combineCounter :: IO.IORef Int
combineCounter = IO.unsafePerformIO (IO.newIORef 0)

{-# NOINLINE projectCounter #-}
projectCounter :: IO.IORef Int
projectCounter = IO.unsafePerformIO (IO.newIORef 0)


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
introducing a `ConstraintKinds` constraint for Var, as well as the addition of `Domain` as
an associated type alongside functions to that achieve the operations on domains that are
required by the generic inference algorithm.

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
