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

-- These warnings are invalid as some of the unused imports and top binds are compiled out
-- based on cpp preprocesor flags
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module GenericInference.ValuationAlgebra
    (
    -- Types and typeclasses
      ValuationFamily (..)
    , Domain
    , Var
    , Valuation
    , IntOrInfinity (..)

    -- Core funcionality
    , project
    , combine

    -- Convenience functions
    , combines1
    , labelSize
    , showDomain

    -- Serialization related typeclasses
    , Binary   -- Must be derived for serialization
    , Typeable -- Must be derived for serialization
    , Generic  -- Must be derived to derive Binary
    , NFData   -- Must be derived for to allow returning as a result of 'Process'.
    , Serializable

    -- Statistics gathering
    , combineCounter
    , projectCounter
    )
where

import           Control.Exception                        (assert)
import qualified Data.Hashable                            as H
import qualified Data.List                                as L
import qualified Data.Map.Lazy                            as M
import qualified Data.Set                                 as S
import           GHC.Base                                 (Constraint)
import qualified GenericInference.Utils                   as U

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

data IntOrInfinity = Int Int | Infinity deriving (Eq, Ord, NFData, Generic, Show)

class ValuationFamily v where

    -- Core operations
    label    :: Var a => v a -> Domain a
    _combine :: Var a => v a -> v a      -> v a
    _project :: Var a => v a -> Domain a -> v a
    identity   :: v a
    isIdentity :: v a -> Bool

    -- [Optional] Default is overridable for performance.
    eliminate :: Var a => v a -> Domain a -> v a
    eliminate v d = project v (S.difference (label v) d)
    notIdentity :: v a -> Bool
    notIdentity = not . isIdentity

    -- [Optional] Solution Construction
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

    -- [Optional] Statistics Gathering
    frameLength :: Var a => a -> v a -> IntOrInfinity


combine :: (ValuationFamily v, Var a) => v a -> v a -> v a
#if !defined(COUNT_OPERATIONS) || !(COUNT_OPERATIONS)
combine v1 v2 = _combine v1 v2
#else
{-# NOINLINE combine #-}
combine v1 v2 = IO.unsafePerformIO $ do
    -- Increment if not a trivial combination.
    case notIdentity v1 && notIdentity v2 of
        True -> U.incrementGlobal combineCounter >> pure ()
        _    -> pure ()
    pure $ _combine v1 v2
#endif

project :: (ValuationFamily v, Var a) => v a -> Domain a -> v a
#if !defined(COUNT_OPERATIONS) || !(COUNT_OPERATIONS)
project v d
    -- If current domain is subset of domain of projection skip projection call for efficency.
    | S.isSubsetOf (label v) d = v
    -- Delegate call to _project but check invariants on return
    | otherwise = _project v d
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
        pure $ _project v d
#endif

labelSize :: Valuation v a => v a -> Int
labelSize = length . label

combines1 :: (Foldable f, ValuationFamily v, Var a) => f (v a) -> v a
combines1 = foldr1 combine

showDomain :: Show a => Domain a -> String
showDomain x = "{" ++ L.intercalate "," (map show (S.toList x)) ++ "}"

--------------------------------------------------------------------------------
-- Counting Operations
--------------------------------------------------------------------------------
{-# NOINLINE combineCounter #-}
combineCounter :: IO.IORef Int
combineCounter = IO.unsafePerformIO (IO.newIORef 0)

{-# NOINLINE projectCounter #-}
projectCounter :: IO.IORef Int
projectCounter = IO.unsafePerformIO (IO.newIORef 0)
