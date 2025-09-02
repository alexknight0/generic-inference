{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeFamilies        #-}

module LocalComputation.ValuationAlgebra.Semiring
    ( SemiringValue (..)
    , SemiringValuation
    , create
    , normalize
    , getRows
    , Valuation
    , findValue
    , mapTableKeys
    , mapVariableValues
    , valueDomains
    )
where

import           LocalComputation.Utils
import           LocalComputation.ValuationAlgebra

import           Control.DeepSeq                                 (NFData)
import           Control.Exception                               (assert)
import           Data.Binary                                     (Binary)
import qualified Data.List                                       as L
import qualified Data.Map                                        as M
import qualified Data.Set                                        as S
import qualified Data.Text.Lazy                                  as LT
import           GHC.Generics
import qualified LocalComputation.Pretty                         as P
import           LocalComputation.ValuationAlgebra.SemiringValue
import           Text.Pretty.Simple                              (pShow,
                                                                  pShowNoColor)

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
data SemiringValuation c b a = Identity { d :: Domain a } | Valuation {
      _rows         :: M.Map (VarAssignment (SemiringValuation c b) a b) c
    , d             :: Domain a
    , _valueDomains :: M.Map a (Domain b)
    , _e            :: Domain a
} deriving (Generic, Binary, NFData)

-- | Returns 'False' if the data structure does not satisfy it's given description.
isWellFormed :: forall a b c. (Ord a, Eq b) => SemiringValuation c b a -> Bool
isWellFormed Identity{} = True
isWellFormed t@Valuation{}
    | M.keysSet t._valueDomains /= t.d = False
    | any (not . validRow) (M.keysSet t._rows) = False
    | length (M.toList t._rows) /= numPermutations = False
    | not (S.disjoint t.d t._e) = False
    | otherwise = True

    where
        validRow :: M.Map a b -> Bool
        validRow row
            | M.keysSet row /= t.d = False
            | any (\(var, value) -> value `notElem` (t._valueDomains M.! var)) (M.toList row) = False
            | otherwise = True

        numPermutations = foldr ((*) . length) 1 (M.elems t._valueDomains)

-- | Creates a valuation, returning 'Nothing' if the given parameters would lead to the creation
-- of a valuation that is not well formed.
create :: (Ord a, Eq b) => M.Map (VarAssignment (SemiringValuation c b) a b) c -> Domain a -> M.Map a (Domain b) -> Domain a -> Maybe (SemiringValuation c b a)
create x y z w
    | isWellFormed result = Just result
    | otherwise = Nothing
    where
        result = Valuation x y z w

-- TODO: Put asserts as a default wrappre for label, combine, project. Then the user
-- instead implements label' combine' and project'.
instance (Ord b, Show b, Show c, SemiringValue c) => Valuation (SemiringValuation c b) where
    type VarAssignment (SemiringValuation c b) a b = M.Map a b

    label x | assertIsWellFormed x = undefined
    label i@Identity{}  = i.d
    label s@Valuation{} = S.union s.d s._e

    combine x y | assertAllWellFormed [x, y] = undefined
    combine i1@Identity{} i2@Identity{} = Identity { d = S.union i1.d i2.d }
    combine i@Identity{}  t@Valuation{} = t { _e = (S.difference (S.union i.d t._e) t.d) }
    combine t@Valuation{} i@Identity{}  = t { _e = (S.difference (S.union i.d t._e) t.d) }
    combine t1@Valuation{} t2@Valuation{} -- (Valuation rowMap1 d1 vD1 e1) (Valuation rowMap2 d2 vD2 e2)
        | null t1._rows = t2 { _e = S.difference (S.union t1._e t2._e) t2.d }
        | null t2._rows = t1 { _e = S.difference (S.union t1._e t2._e) t2.d }
        | otherwise = Valuation newRows newDomain newValueDomain newExtension
        where
            newRows = fromListAssertDisjoint [(unionAssert' a1 a2, v1 `multiply` v2) | (a1, v1) <- M.toList t1._rows, (a2, v2) <- M.toList t2._rows, hasSameValueForSharedVariables a1 a2]
            newDomain = S.union t1.d t2.d
            newValueDomain = unionAssert2' t1._valueDomains t2._valueDomains
            newExtension = S.difference (S.unions [t1.d, t2.d, t1._e, t2._e]) (S.unions [t1.d, t2.d])

            -- Asserts colliding keys have same value
            unionAssert' :: (Ord a) => M.Map a b -> M.Map a b -> M.Map a b
            unionAssert' = M.unionWith (\v1 v2 -> assert (v1 == v2) v1)

            -- TODO: Fix redunant function
            -- I think the reason it can't infer the type here properly is TypeFamilies implying MonoLocalBinds;
            --  moving this up to the global scope should fix it if this is the case!
            unionAssert2' :: (Ord a) => M.Map a (Domain b) -> M.Map a (Domain b) -> M.Map a (Domain b)
            unionAssert2' = M.unionWith (\v1 v2 -> assert (v1 == v2) v1)

    project x _ | assertIsWellFormed x = undefined
    project x y | assert (S.isSubsetOf y (label x)) False = undefined
    project i@Identity{}  d = i { d = d }
    project t@Valuation{} d = t { _rows         = M.mapKeysWith add projectDomain1 t._rows
                                , d             = projectDomain3 t.d
                                , _valueDomains = projectDomain2 t._valueDomains
                                , _e            = projectDomain3 t._e
                                }
        where
            -- I know. Try change it.
            projectDomain1 = M.filterWithKey (\k _ -> k `elem` d)
            projectDomain2 = M.filterWithKey (\k _ -> k `elem` d)
            projectDomain3 = S.filter (\k -> k `elem` d)

    identity = Identity

    -- TODO: Implement (probably can be implemented through a default implementation?)
    eliminate x _ | assertIsWellFormed x = undefined
    eliminate v x = project v (S.difference (label v) x)

    -- frame x | assertIsWellFormed x = undefined
    -- frame _ = error "Not implemented."

toTable :: (Show a, Show b, Show c) => SemiringValuation a b c -> Table
toTable Identity{}    = error "Not Implemented"
toTable v@Valuation{} = Table headings rows
    where
        assignedValues a = map show $ M.elems a
        keys a = map show $ M.keys a

        headings = (head $ map (\(assignment, _) -> keys assignment) $ M.toAscList v._rows)
                ++ ["Probability"]
        rows     = map (\(assignment, value) -> assignedValues assignment ++ [show value]) $ M.toAscList v._rows

instance (Show a, Show b, Show c) => Show (SemiringValuation a b c) where
    show Identity{}    = "Identity"
    show v@Valuation{} = P.showTable $ toTable v

    -- show v@Valuation{} = L.intercalate "\n" [showAssignment assignment ++ " " ++ show value
    --                                             | (assignment, value) <- M.toAscList v._rows]
    --      where
    --          showAssignment a = "["
    --                          ++ L.intercalate ", " [show (var, value) | (var, value) <- M.toAscList a]
    --                          ++ "]"

-- show v@Valuation{} = concat $ L.intersperse "\n" $ map show $ M.elems v._rows

foobar :: ()
foobar = undefined
{-

>>> getRows [("Barry", [1, 2]), ("James", [3,4])] [2,4,5,6]
"Barry" "James" Probability
1       1       2
1       2       2
2       2
2

-}

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
getRows :: forall c b a. (Ord a, Ord b) => [(a, [b])] -> [c] -> SemiringValuation c b a
getRows vars ps = assert' isWellFormed $ Valuation rMap d valueDomains extension
    where
        rMap = fromListAssertDisjoint $ zipAssert (vPermutations vars) ps
        d = fromListAssertDisjoint' $ map fst vars
        valueDomains = fromListAssertDisjoint (map (\(v, values) -> (v, fromListAssertDisjoint' values)) vars)
        extension = S.empty

        vPermutations :: [(a, [b])] -> [VarAssignment (SemiringValuation c b) a b]
        vPermutations xs = map fromListAssertDisjoint $ vPermutations' xs
            where
                vPermutations' :: [(a, [b])] -> [[(a, b)]]
                vPermutations' [] = [[]]
                vPermutations' ((v, vVals) : vs) = [(v, vVal) : rest | vVal <- vVals, rest <- vPermutations' vs]

-- TODO: Wait isn't this just M.intersectionWith const m1 m2  == M.intersectionWith (flip const) m1 m2
hasSameValueForSharedVariables :: (Ord a, Eq b) => M.Map a b -> M.Map a b -> Bool
hasSameValueForSharedVariables xs ys = all (\k -> xs M.! k == ys M.! k) sharedKeys
    where
        sharedKeys = S.toList $ S.intersection (M.keysSet xs) (M.keysSet ys)

-- | Returns the value of the given variable arrangement. Unsafe.
findValue :: (Ord a, Ord b) => VarAssignment (SemiringValuation c b) a b -> SemiringValuation c b a -> c
findValue x (Valuation rowMap _ _ _) = rowMap M.! x
findValue _ (Identity _) = error "findProbability: Attempted to read value from an identity valuation."

mapTableKeys :: (Ord b, Ord c) => (a -> b) -> SemiringValuation d c a -> SemiringValuation d c b
mapTableKeys f (Valuation rMap d vD e) = Valuation (M.mapKeys (M.mapKeys f) rMap) (setMap f d) (M.mapKeys f vD) (setMap f e)
mapTableKeys f (Identity x) = Identity (setMap f x)

mapVariableValues :: (Ord b, Ord c) => (a -> b) -> SemiringValuation d a c -> SemiringValuation d b c
mapVariableValues _ v@Identity{}  = Identity v.d
mapVariableValues f v@Valuation{} = v { _rows = M.mapKeys (M.map f) v._rows
                                      , _valueDomains = M.map (S.map f) v._valueDomains
                                     }

normalize :: (Fractional c) => SemiringValuation c b a -> SemiringValuation c b a
normalize (Identity x) = Identity x
normalize (Valuation rowMap d vD e) = Valuation (M.map (/ sumOfAllXs) rowMap) d vD e
    where
        sumOfAllXs = sum $ M.elems rowMap

assertAllWellFormed :: (Foldable t, Ord a, Eq b) => t (SemiringValuation c b a) -> Bool
assertAllWellFormed = any (\x -> assert (isWellFormed x) False)

valueDomains :: SemiringValuation c b a -> M.Map a (Domain b)
valueDomains = (._valueDomains)

assertIsWellFormed :: (Ord a, Eq b) => SemiringValuation c b a -> Bool
assertIsWellFormed x = assert (isWellFormed x) False
