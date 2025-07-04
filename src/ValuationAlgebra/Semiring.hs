{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ValuationAlgebra.Semiring
    ( SemiringValue (add, multiply, zero, one)
    , getRows
    , showAsRows
    , SemiringValuation (Table, Identity)
    , Row (Row)
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
import           GHC.Generics
import           ValuationAlgebra.SemiringValue



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
data SemiringValuation a b c = Table [Row a b c] | Identity deriving (Generic, Binary)

-- Don't be suprised if you need to put (Enum, bounded) on 'b'.
instance (SemiringValue a) => Valuation (SemiringValuation a) where
    label Identity        = empty
    label (Table [])      = empty
    label (Table (x : _)) = M.keysSet (variables x)

    combine Identity x = x
    combine x Identity = x
    combine (Table []) _ = Table []
    combine _ (Table []) = Table []
    combine (Table (x:xs)) (Table (y:ys)) = Table $
            [Row (unionUnsafe (variables a) (variables b)) (value a `multiply` value b)
                | a <- (x:xs), b <- (y:ys), sharedVariablesAreSameValue numSharedVars a b]

        where
            numSharedVars = fromIntegral . length $ intersection (M.keysSet (variables x)) (M.keysSet (variables y))

    project Identity _ = Identity
    project (Table xs) domain = Table $ nubWithBy (\(Row vs _) -> vs) addRows $ map (\(Row vs p) -> Row (projectedDomain vs) p) xs
        where
            projectedDomain = M.filterWithKey (\k _ -> k `elem` domain)
            addRows (Row vs1 p1) (Row vs2 p2) = assert (vs1 == vs2) $ Row vs1 (p1 `add` p2)

    identity = Identity

instance (Show a, Show b, Show c) => Show (SemiringValuation a b c) where
    show = showAsRows

showAsRows :: (Show a, Show b, Show c) => SemiringValuation a b c -> String
showAsRows (Table xs) = "\n------ Table ------\n"
                     ++ concatMap (\(Row vs p) -> show vs ++ "   " ++ show p ++ "\n") xs
                     ++ "-------------------\n"
showAsRows Identity = "\n------ Table ------\n"
                   ++ "Identity"
                   ++ "-------------------\n"


-- An inefficent storage format, but we should get a working implementation first.
data Row a b c = Row
    {
        variables :: Variables b c,
        value     :: a
    }
    deriving (Show, Generic, Binary)

type Variable a b = (a, b)
type Variables a b = M.Map a b

getRows :: forall a b c. (Ord b) => [(b, [c])] -> [a] -> SemiringValuation a b c
getRows vars ps = Table $ zipWithAssert Row (vPermutations vars) ps
    where

        vPermutations :: [(b, [c])] -> [Variables b c]
        vPermutations xs = map fromListAssertDisjoint $ vPermutations' xs
            where
                vPermutations' :: [(b, [c])] -> [[Variable b c]]
                vPermutations' [] = [[]]
                vPermutations' ((v, vVals) : vs) = [(v, vVal) : rest | vVal <- vVals, rest <- vPermutations' vs]


-- Returns true iff the two rows should be combined as a part of a combine operation.
-- The rows should be combined if all of their shared variables are the same value.
sharedVariablesAreSameValue :: (Ord b, Ord c) => Integer -> Row a b c -> Row a b c -> Bool
sharedVariablesAreSameValue numSharedVariables x y =
        fromIntegral (length (intersection (S.fromList $ M.assocs $ variables x) (S.fromList $ M.assocs $ variables y))) == numSharedVariables

-- unsafe
findValue :: (Eq a, Eq b) => Variables a b -> SemiringValuation c a b -> c
findValue x (Table rows) = (\(Row _ p) -> p) $ findAssertSingleMatch (\(Row vs _) -> vs == x) rows
findValue _ Identity = error "findProbability: Attempted to read value from an identity valuation."

mapTableKeys :: (Ord b) => (a -> b) -> SemiringValuation d a c -> SemiringValuation d b c
mapTableKeys f (Table xs) = Table $ map (\(Row vs p) -> Row (M.mapKeys f vs) p) xs
mapTableKeys _ Identity = Identity
