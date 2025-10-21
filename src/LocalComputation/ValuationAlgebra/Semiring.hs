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
    , create
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

import           GHC.Stack                                        (HasCallStack)
import qualified LocalComputation.Potential                       as P
import qualified LocalComputation.ValuationAlgebra                as V

-- TODO: Might be able to simplify this definition if a better notion of the identity element is created.
-- Or this might just slow the performance down.
-- TODO: Instead of extension could have 0 sized arrangement entry?
-- TODO: Update documentation

{- | Valuation for a semiring valuation algebra.

This can be thought of as a table, with each entry as a row.
For example:

    rowMap:  [([("Country", "Australia"), ("Item", "Gum")],   2),
              ([("Country", "Australia"), ("Item", "Water")], 4),
              ([("Country", "Korea"),     ("Item", "Gum")],   6),
              ([("Country", "Korea"),     ("Item", "Water")], 8),
            ]

    varDomain:        ["Country", "Item"]
    varToValueDomain: [("Country", ["Australia", "Korea"]),
                       ("Item", ["Gum", "Water"])]
    extension:        []

Represents:

    Country        Item

    Australia      Gum             2
    Australia      Water           4
    Korea          Gum             6
    Korea          Water           8


rows :: M.Map (M.Map a b) c,

    A mapping from variable arrangements 'M.Map a b' to values 'c'.
    Each variable arrangement is a permutation of an assignment of
    each element of the domain 'varDomain' to a value from
    'varToValueDomain'. All permutations can be found in rowMap.

d :: Domain a,

    All the variables that are included in a variable arrangement.

valueDomains :: M.Map a (Domain b),

    The possible values each variable can take in a variable arrangement.

e :: Domain a

    An extension to 'varDomain' that extends the domain of the valuation
    but without adding any extra rows. This could be thought of as adding
    an extra variable to the 'varDomain' where the variable only has one
    possible value, so no extra permutations are generated.

    This is required as combination of a valuation with an identity element
    must result in a label that contains the union of identity element's
    domain and the other element's domain. If this does not occur, it will
    appear that projections to domains that are outside a valuation's label
    are occuring.

Note that a restriction of this form is that the type of the value of each
variable in a variable arrangement is the same.
-}
data Valuation c b a = Identity | Valuation {
      _p :: P.Potential a b c
} deriving (Generic, Binary, NFData)

-- -- | Accessor for domain. Not O(1)!
-- instance HasField "d" (Valuation c b a) (Domain a) where
--     getField i@Identity{}  = S.empty
--     getField v@Valuation{} = M.keysSet $ P.toFrames v._p

-- | Creates a valuation, returning 'Nothing' if the given parameters would lead to the creation
-- of a valuation that is not well formed.
create :: P.Potential a b c -> Maybe (Valuation c b a)
create p = Just $ unsafeCreate p

unsafeCreate :: P.Potential a b c -> Valuation c b a
unsafeCreate p = Valuation p

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

    identity _ = Identity

    isIdentity Identity = True
    isIdentity _        = False

    satisfiesInvariants = const True

    type VarAssignment (Valuation c b) a = ProxiedMap c a b
    combineAssignments  = error "Not Implemented"
    projectAssignment   = error "Not Implemented"
    configurationExtSet = error "Not Implemented"
    emptyAssignment     = error "Not Implemented"

    frameLength _   Identity = error "Unknown frame length"
    frameLength var t        = V.Int $ S.size $ P.frame var t._p


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
getRows :: forall c b a. (HasCallStack, Ord a, Ord b) => [(a, [b])] -> [c] -> Valuation c b a
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

-- normalize :: (Var a, Show b, Ord b, Show c, SemiringValue c, Fractional c)
--     => Valuation c b a -> Valuation c b a
-- normalize v = assertInvariants $ _normalize v
--
-- _normalize :: (Fractional c) => Valuation c b a -> Valuation c b a
-- _normalize (Identity x) = Identity x
-- _normalize (Valuation rowMap d vD e) = Valuation (M.map (/ sumOfAllXs) rowMap) d vD e
--     where
--         sumOfAllXs = sum $ M.elems rowMap

toFrames :: Valuation c b a -> M.Map a (Domain b)
toFrames Identity = error "Called 'toFrames' on identity element"
toFrames v        = P.toFrames v._p

fromPermutationMap :: (Ord a, Ord b) => M.Map (M.Map a b) c -> Valuation c b a
fromPermutationMap m = unsafeCreate $ P.fromPermutationMap m
