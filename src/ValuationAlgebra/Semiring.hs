{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ValuationAlgebra.Semiring
    ( SemiringValue (add, multiply, zero, one)
    , SemiringValuation
    , create
    , normalize
    , getRows
    , showAsRows
    , Valuation
    , Variables
    , Variable
    , findValue
    , mapTableKeys
    )
where

import           Utils
import           ValuationAlgebra

import           Control.Exception              (assert)
import           Data.Binary                    (Binary)
import qualified Data.Map                       as M
import           Data.Set                       (empty, intersection)
import qualified Data.Set                       as S
import           Debug.Trace                    (trace)
import           GHC.Generics
import           ValuationAlgebra.SemiringValue

isWellFormed :: forall a b c. (Ord a, Eq b) => SemiringValuation c a b -> Bool
isWellFormed (Valuation rows d valueDomains vacuousExtension)
    | M.keysSet valueDomains /= d = False
    | any (not . validRow) (M.keysSet rows) = False
    | length (M.toList rows) /= numPermutations = False
    | not (S.disjoint d vacuousExtension) = False
    | otherwise = True

    where
        validRow :: M.Map a b -> Bool
        validRow row
            | M.keysSet row /= d = False
            | any (\(var, value) -> value `notElem` (valueDomains M.! var)) (M.toList row) = False
            | otherwise = True

        numPermutations = foldr ((*) . length) 1 (M.elems valueDomains)

{-
In BValColumns, the first parameter is the variable, the second parameter
is a list of the conditions, and the third parameter is the list of probabilities,
where probabilities are ordered as follows:

    var   A   B   Probability

     0    0   0       0.7       P(var == 0 | A == 0 && B == 0)

     0    0   1       0.4       P(var == 0 | A == 0 && B == 1)

     0    0   2       0.3       P(var == 0 | A == 0 && B == 2)

     0    1   0       0.6       P(var == 0 | A == 1 && B == 0)

     .    .   .        .                      .
     .    .   .        .                      .
     .    .   .        .                      .

BValRows stores the equivalent information except as what is essentially as a tuple of each
row of the table instead.

Note that neither form allows the range of values that can be stored in a certain field differ across fields;
i.e. if 'A' can range from 0 to 1, then B must also range from 0 to 1.

BValColumns stores no redundant information, while BValRows stores a heap of redundant information,
but allows accessing this information in a more haskell-like manner.
-}
data SemiringValuation c a b = Valuation
    {
        rowMap           :: M.Map (M.Map a b) c,
        varDomain        :: Domain a,
        varToValueDomain :: M.Map a (Domain b),
        -- Vacuous extension to the variable domain.
        extension        :: Domain a
    } | Identity (Domain a) deriving (Generic, Binary)

create :: (Ord a, Eq b) => M.Map (M.Map a b) c -> Domain a -> M.Map a (Domain b) -> Domain a -> Maybe (SemiringValuation c a b)
create x y z w
    | isWellFormed result = Just result
    | otherwise = Nothing
    where
        result = Valuation x y z w

-- create' :: (Ord a, Eq b) => M.Map (M.Map a b) c -> Maybe (SemiringValuation c a b)
-- create' rowMap
--     | null rowMap = assert isWellFormed $ create (M.empty) (S.empty) (M.empty) (S.empty)
--     | isWellFormed (create rowMap d
--     where
--         d = M.keysSet (head $ M.keys rowMap)
--         e = S.empty


-- Don't be suprised if you need to put (Enum, bounded) on 'b'.
instance (Show c, SemiringValue c) => Valuation (SemiringValuation c) where
    label (Identity d)                       = d
    label (Valuation {varDomain, extension}) = S.union varDomain extension

    combine (Identity d1) (Identity d2) = Identity (S.union d1 d2)
    combine (Identity d1) (Valuation x y z d2) = Valuation x y z (S.union d1 d2)
    combine (Valuation x y z d2) (Identity d1) = Valuation x y z (S.union d1 d2)
    combine (Valuation rowMap1 d1 vD1 e1) (Valuation rowMap2 d2 vD2 e2)
        | null rowMap1 = Valuation rowMap2 d2 vD2 (S.difference (S.union e1 e2) d2)
        | null rowMap2 = Valuation rowMap1 d1 vD1 (S.difference (S.union e1 e2) d1)
        | otherwise = Valuation newRows newDomain newValueDomain newExtension
        where
            newRows = fromListAssertDisjoint [(unionAssert' a1 a2, v1 `multiply` v2) | (a1, v1) <- M.toList rowMap1, (a2, v2) <- M.toList rowMap2, hasSameValueForSharedVariables a1 a2]
            newDomain = S.union d1 d2
            newValueDomain = unionAssert' vD1 vD2
            newExtension = S.difference (S.unions [d1, d2, e1, e2]) (S.unions [d1, d2])

            -- Asserts colliding keys have same value
            unionAssert' :: (Ord a, Eq b) => M.Map a b -> M.Map a b -> M.Map a b
            unionAssert' = M.unionWith (\v1 v2 -> assert (v1 == v2) v1)

    project (Identity _) y = Identity y
    project (Valuation rMap _ vD _) newD = Valuation (M.mapKeysWith add projectDomain rMap) newD (projectDomain vD) newD
        where
            projectDomain = M.filterWithKey (\k _ -> k `elem` newD)

    identity = Identity

instance (Show a, Show b, Show c) => Show (SemiringValuation a b c) where
    show = showAsRows

-- TODO: finish
showAsRows :: (Show a, Show b, Show c) => SemiringValuation a b c -> String
showAsRows (Valuation {}) = "\n------ Table ------\n"
                     ++ "-------------------\n"
showAsRows (Identity _) = "\n------ Table ------\n"
                   ++ "Identity"
                   ++ "-------------------\n"


type Variable a b = (a, b)
type Variables a b = M.Map a b

getRows :: forall a b c. (Ord b, Ord c) => [(b, [c])] -> [a] -> SemiringValuation a b c
getRows vars ps = Valuation rMap d valueDomains extension
    where
        rMap = fromListAssertDisjoint $ zipAssert (vPermutations vars) ps
        d = fromListAssertDisjoint' $ map fst vars
        valueDomains = fromListAssertDisjoint (map (\(v, values) -> (v, fromListAssertDisjoint' values)) vars)
        extension = S.empty

        vPermutations :: [(b, [c])] -> [Variables b c]
        vPermutations xs = map fromListAssertDisjoint $ vPermutations' xs
            where
                vPermutations' :: [(b, [c])] -> [[Variable b c]]
                vPermutations' [] = [[]]
                vPermutations' ((v, vVals) : vs) = [(v, vVal) : rest | vVal <- vVals, rest <- vPermutations' vs]

hasSameValueForSharedVariables :: (Ord a, Eq b) => M.Map a b -> M.Map a b -> Bool
hasSameValueForSharedVariables xs ys = all (\k -> xs M.! k == ys M.! k) sharedKeys
    where
        sharedKeys = S.toList $ S.intersection (M.keysSet xs) (M.keysSet ys)

-- unsafe
findValue :: (Ord a, Ord b) => Variables a b -> SemiringValuation c a b -> c
findValue x (Valuation {rowMap}) = rowMap M.! x
findValue _ (Identity _) = error "findProbability: Attempted to read value from an identity valuation."

mapTableKeys :: (Ord b, Ord c) => (a -> b) -> SemiringValuation d a c -> SemiringValuation d b c
mapTableKeys f (Valuation rMap d vD e) = Valuation (M.mapKeys (M.mapKeys f) rMap) (setMap f d) (M.mapKeys f vD) (setMap f e)
mapTableKeys f (Identity x) = Identity (setMap f x)

normalize :: (Fractional c) => SemiringValuation c a b -> SemiringValuation c a b
normalize (Identity x) = Identity x
normalize (Valuation rowMap d vD e) = Valuation (M.map (/ sumOfAllXs) rowMap) d vD e
    where
        sumOfAllXs = sum $ M.elems rowMap
