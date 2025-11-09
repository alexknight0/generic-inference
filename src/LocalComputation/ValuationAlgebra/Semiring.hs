{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeFamilies        #-}

module LocalComputation.ValuationAlgebra.Semiring
    ( SemiringValue (..)
    , Valuation
    , unsafeCreate
    , fromPermutationMap
    , getRows
    , ValuationFamily
    , findValue
    , mapTableKeys
    , mapVariableValues
    , toFrames
    )
where

import           LocalComputation.ValuationAlgebra                hiding
                                                                  (Valuation)

import qualified Data.Map                                         as M
import qualified Data.Set                                         as S
import qualified LocalComputation.Pretty                          as P
import           LocalComputation.ValuationAlgebra.Semiring.Value

import qualified LocalComputation.Potential                       as P
import qualified LocalComputation.ValuationAlgebra                as V

{- | Valuation for a semiring valuation algebra. -}
data Valuation c b a = Identity | Valuation {
      _p :: P.Potential a b c
} deriving (Generic, Binary, NFData)

unsafeCreate :: P.Potential a b c -> Valuation c b a
unsafeCreate p = Valuation p

-- | A map that proxies an extra type such that it can be used to implement VarAssignment.
type ProxiedMap c a b = M.Map a b

instance (Ord b, Show b, Show c, SemiringValue c) => ValuationFamily (Valuation c b) where

    label Identity      = S.empty
    label s@Valuation{} = M.keysSet $ P.toFrames s._p

    _combine Identity Identity = Identity
    _combine Identity t = t
    _combine t Identity = t
    _combine t1 t2
        | P.null t1._p = t2
        | P.null t2._p = t1
        | otherwise = unsafeCreate newP
        where
            newP = P.combine multiply t1._p t2._p

    _project Identity _ = Identity
    _project t d        = unsafeCreate $ P.project add t._p d

    identity = Identity

    isIdentity Identity = True
    isIdentity _        = False

    type VarAssignment (Valuation c b) a = ProxiedMap c a b

    frameLength _   Identity = error "Unknown frame length"
    frameLength var t        = V.Int $ S.size $ P.frame var t._p

    combineAssignments  = error "Not Implemented"
    projectAssignment   = error "Not Implemented"
    configurationExtSet = error "Not Implemented"
    emptyAssignment     = error "Not Implemented"


toTable :: (Show a, Show b, Show c, Ord b, Ord c) => Valuation a b c -> P.Table
toTable Identity = error "Not Implemented"
toTable v        = P.unsafeTable headings rows
    where
        assignedValues a = map show $ M.elems a

        headings = (map show . M.keys . P.toFrames $ v._p) ++ ["Probability"]
        rows     = map (\(assignment, value) -> assignedValues assignment ++ [show value]) $ M.toAscList
                                                                                           $ P.permutationMap v._p

instance (Show a, Show b, Show c, Ord b, Ord c) => Show (Valuation a b c) where
    show Identity = "Identity"
    show v        = P.showTable $ toTable v

{-
The first parameter is an association list mapping a variable to a list of values that the variable can take.
The list of values that the variable can take should not contain duplicate elements, and the association list
should not contain duplicate entries for a key. The second parameter is a list of values assigned to each row.

The values of the association list are permuted based on the order they are found in the list, and then zipped
with the values of the second parameter. For example:

    > getRows [("Country", ["Australia", "Korea"]), ("Item", ["Gum", "Water"])] [2,4,6,8]

    < Country        Item
    <
    < Australia      Gum             2
    < Australia      Water           4
    < Korea          Gum             6
    < Korea          Water           8
-}
getRows :: forall c b a. (Ord a, Ord b) => [(a, [b])] -> [c] -> Valuation c b a
getRows vars ps = unsafeCreate $ P.unsafeFromList vars ps

-- | Returns the value of the given variable arrangement. Unsafe.
findValue :: (Ord a, Ord b) => VarAssignment (Valuation c b) a -> Valuation c b a -> c
findValue _ Identity = error "findProbability: Attempted to read value from an identity valuation."
findValue x v        = P.unsafeGetValue v._p x

mapTableKeys :: (Ord a1, Ord a2, Ord b) => (a1 -> a2) -> Valuation c b a1 -> Valuation c b a2
mapTableKeys _ Identity = Identity
mapTableKeys f v        = unsafeCreate $ P.mapVariables f v._p

mapVariableValues :: (Ord a, Ord b1, Ord b2) => (b1 -> b2) -> Valuation c b1 a -> Valuation c b2 a
mapVariableValues _ Identity = Identity
mapVariableValues f v        = unsafeCreate $ P.mapFrames f v._p

toFrames :: Valuation c b a -> M.Map a (Domain b)
toFrames Identity = error "Called 'toFrames' on identity element"
toFrames v        = P.toFrames v._p

fromPermutationMap :: (Ord a, Ord b) => M.Map (M.Map a b) c -> Valuation c b a
fromPermutationMap m = unsafeCreate $ P.fromPermutationMap m
