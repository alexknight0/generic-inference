{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LocalComputation.Inference.JoinTree.Tree (

    -- Nodes
      Node (id, v, t)
    , node
    , changeContent
    , NodeType (Valuation, Query, Union, Projection)
    , Id

    -- Join Trees
    , JoinTree
    , satisfiesInvariants
    , fromGraph
) where

import           GHC.Records                        (HasField, getField)
import           LocalComputation.ValuationAlgebra  hiding (assertInvariants,
                                                     satisfiesInvariants)

import qualified Algebra.Graph                      as G
import qualified Algebra.Graph.Acyclic.AdjacencyMap as G (toAcyclic)
import qualified Algebra.Graph.ToGraph              as G (dfsForest, reachable,
                                                          toAdjacencyMap,
                                                          topSort)
import qualified Algebra.Graph.Undirected           as UG
import           Control.Exception                  (assert)
import qualified Data.Bifunctor                     as B
import qualified Data.List                          as L
import qualified Data.Map                           as M
import           Data.Maybe                         (fromJust, isJust)
import qualified Data.Set                           as S
import           Data.Text.Lazy                     (unpack)
import qualified LocalComputation.Utils             as U
import           Numeric.Natural                    (Natural)
import           Text.Pretty.Simple                 (pShow)

--------------------------------------------------------------------------------
-- Nodes
--------------------------------------------------------------------------------

data Node v = Node {
      id :: Id
    , v  :: v
    , t  :: NodeType
} deriving (Generic, Typeable, Binary)

type Id = Integer

data NodeType = Valuation | Query | Union | Projection deriving (Show, Generic, Binary, Enum, Bounded, Eq)

node :: Id -> v -> NodeType -> Node v
node = Node

changeContent :: Node a -> a -> Node a
changeContent n v = n { v = v }

-- | Accessor for the domain of the valuation.  Equivalent to calling `label` on the valuation.
-- __Warning__: Not necessarily O(1).
instance (Valuation v, Ord a, Show a) => HasField "d" (Node (v a)) (Domain a) where
    getField m = label m.v

-- TODO: Maybe remove these.
instance Eq (Node v) where
    x == y = x.id == y.id

instance Ord (Node v) where
    x <= y = x.id <= y.id

instance (Valuation v, Ord a, Show a) => Show (Node (v a)) where
    show n = unpack $ pShow (n.id, n.d)

--------------------------------------------------------------------------------
-- Join trees
--------------------------------------------------------------------------------

newtype JoinTree v = UnsafeJoinTree { g :: G.Graph (Node v) }

-- | Converts a graph into a join tree.
--
-- __Warning__: Unsafe - will check some invariants associated with a join tree
-- and throw an error if the graph doesn't satisfy these invariants.
fromGraph :: G.Graph (Node v) -> JoinTree v
fromGraph = U.assertP satisfiesInvariants . UnsafeJoinTree


--------------------------------------------------------------------------------
-- Invariants
--------------------------------------------------------------------------------

isAcyclic :: JoinTree v -> Bool
isAcyclic t = isJust . G.toAcyclic . G.toAdjacencyMap $ t.g

satisfiesInvariants :: JoinTree v -> Bool
satisfiesInvariants = isAcyclic



