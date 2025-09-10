{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}

module LocalComputation.Inference.JoinTree.Tree
    ( Node (id, v, t)
    , node
    , changeContent
    , NodeType (Valuation, Query, Union, Projection)
    , Id
    )
where

import           GHC.Records                       (HasField, getField)
import           LocalComputation.ValuationAlgebra

import           Data.Text.Lazy                    (unpack)
import           Text.Pretty.Simple                (pShow)

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

instance Eq (Node v) where
    x == y = x.id == y.id

instance Ord (Node v) where
    x <= y = x.id <= y.id

instance (Valuation v, Ord a, Show a) => Show (Node (v a)) where
    show n = unpack $ pShow (n.id, n.d)

--------------------------------------------------------------------------------
-- Join Trees
--------------------------------------------------------------------------------

-- TODO: Should have a seperate data structure for join trees. That way we
-- not only can better assert the tree is in a set structure, but we can
-- also potentially get better performance for operations such as
-- "find a node with *this* id".

-- TODO: A join tree is really a join forest. Once we create a proper data
-- structure for join trees with invariants, this will be more evident.

newtype JoinTree v = JoinTree { t :: Graph (Node v) }




