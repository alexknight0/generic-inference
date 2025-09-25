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
import qualified LocalComputation.Utils                           as U
import           LocalComputation.ValuationAlgebra.Semiring.Value

import           GHC.Records                                      (HasField,
                                                                   getField)

import           GHC.Stack                                        (HasCallStack)
import qualified LocalComputation.Potential                       as P

-- TODO: Might be able to simplify this definition if a better notion of the identity element is created.
-- Or this might just slow the performance down.
-- TODO: Instead of extension could have 0 sized arrangement entry?

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
data Valuation c b a = Identity { _d :: Domain a } | Valuation {
      _p :: P.Potential a b c
    , _e :: Domain a
} deriving (Generic, Binary, NFData)

-- | Accessor for domain. Not O(1)!
instance HasField "d" (Valuation c b a) (Domain a) where
    getField i@Identity{}  = i._d
    getField v@Valuation{} = M.keysSet $ P.toFrames v._p

-- | Returns 'False' if the data structure does not satisfy it's given description.
isWellFormed :: forall a b c. (Ord a) => Valuation c b a -> Bool
isWellFormed Identity{} = True
isWellFormed t@Valuation{}
    | not (S.disjoint t.d t._e) = False
    | otherwise = True

-- | Creates a valuation, returning 'Nothing' if the given parameters would lead to the creation
-- of a valuation that is not well formed.
create :: (Ord a) => P.Potential a b c -> Domain a -> Maybe (Valuation c b a)
create x y
    | isWellFormed result = Just result
    | otherwise = Nothing
    where
        result = Valuation x y

unsafeCreate :: (Ord a) => P.Potential a b c -> Domain a -> Valuation c b a
unsafeCreate p e = U.assertP isWellFormed $ Valuation p e

instance (Ord b, Show b, Show c, SemiringValue c) => ValuationFamily (Valuation c b) where
    type VarAssignment (Valuation c b) a b = M.Map a b

    label i@Identity{}  = i.d
    label s@Valuation{} = S.union s.d s._e

    _combine i1@Identity{} i2@Identity{} = Identity { _d = S.union i1.d i2.d }
    _combine i@Identity{}  t@Valuation{} = t { _e = (S.difference (S.union i.d t._e) t.d) }
    _combine t@Valuation{} i@Identity{}  = t { _e = (S.difference (S.union i.d t._e) t.d) }
    _combine t1@Valuation{} t2@Valuation{}
        | P.null t1._p = t2 { _e = S.difference (S.union t1._e t2._e) t2.d }
        | P.null t2._p = t1 { _e = S.difference (S.union t1._e t2._e) t2.d }
        | otherwise = Valuation newP newExtension
        where
            newP = P.combine multiply t1._p t2._p
            newExtension = S.difference (S.unions [t1.d, t2.d, t1._e, t2._e]) (S.unions [t1.d, t2.d])

    _project i@Identity{}  d = i { _d = d }
    _project t@Valuation{} d = t { _p = P.project add t._p d
                                 , _e = projectDomain3 t._e
                                }
        where
            projectDomain3 = S.filter (\k -> k `elem` d)

    identity = Identity

    satisfiesInvariants = isWellFormed

toTable :: (Show a, Show b, Show c, Ord b, Ord c) => Valuation a b c -> P.Table
toTable Identity{}    = error "Not Implemented"
toTable v@Valuation{} = P.unsafeTable headings rows
    where
        assignedValues a = map show $ M.elems a

        headings = (map show . M.keys . P.toFrames $ v._p) ++ ["Probability"]
        rows     = map (\(assignment, value) -> assignedValues assignment ++ [show value]) $ M.toAscList
                                                                                           $ P.permutationMap v._p

instance (Show a, Show b, Show c, Ord b, Ord c) => Show (Valuation a b c) where
    show Identity{}    = "Identity"
    show v@Valuation{} = P.showTable $ toTable v

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
getRows vars ps = U.assertP isWellFormed $ _getRows vars ps

_getRows :: forall c b a. (HasCallStack, Ord a, Ord b) => [(a, [b])] -> [c] -> Valuation c b a
_getRows vars ps = Valuation (P.unsafeFromList vars ps) S.empty

-- | Returns the value of the given variable arrangement. Unsafe.
findValue :: (Ord a, Ord b) => VarAssignment (Valuation c b) a b -> Valuation c b a -> c
findValue x v@Valuation{} = P.unsafeGetValue v._p x
findValue _ (Identity _) = error "findProbability: Attempted to read value from an identity valuation."

mapTableKeys :: (Ord a1, Ord a2, Ord b) => (a1 -> a2) -> Valuation c b a1 -> Valuation c b a2
mapTableKeys f i@Identity{}  = Identity (S.map f i.d)
mapTableKeys f v@Valuation{} = unsafeCreate (P.mapVariables f v._p) (S.map f v._e)

mapVariableValues :: (Ord a, Ord b1, Ord b2) => (b1 -> b2) -> Valuation c b1 a -> Valuation c b2 a
mapVariableValues _ i@Identity{}  = Identity i.d
mapVariableValues f v@Valuation{} = unsafeCreate (P.mapFrames f v._p) v._e

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
toFrames Identity{}    = error "Called 'toFrames' on identity element"
toFrames v@Valuation{} = P.toFrames v._p

fromPermutationMap :: (Ord a, Ord b) => M.Map (M.Map a b) c -> Valuation c b a
fromPermutationMap m = unsafeCreate (P.fromPermutationMap m) S.empty
