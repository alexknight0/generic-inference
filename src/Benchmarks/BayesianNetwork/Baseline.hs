module Benchmarks.BayesianNetwork.Baseline
    ( runQuery, createNetwork, runQueries )
where

import qualified Data.Map                                   as M
import qualified LocalComputation.Instances.BayesianNetwork as B
import           Numeric.Probability.Distribution           ((?=<<), (??))
import           Numeric.Probability.Example.Bayesian       (PState, SPred,
                                                             STrans, State,
                                                             event, happens,
                                                             network)
import qualified Numeric.Probability.Example.Bayesian       as P (Probability)

import           LocalComputation.Utils                     (divAssert,
                                                             findAssertSingleMatch,
                                                             zipAssert)

type Network a = PState a
type Potential a = STrans a

createNetwork :: (Eq a) => [([a], [B.Probability])] -> Network a
createNetwork xs = network $ map (uncurry createPotential) xs

createPotential :: (Eq a) => [a] -> [B.Probability] -> Potential a
createPotential [] _ = error "Can't create potential with no variables"
createPotential (v:vs) ps = potential v vs (drop (length ps `divAssert` 2) $ map (toRational . (\(B.P x) -> x)) ps)

doesNotHappen :: Eq a => SPred a
doesNotHappen x y = not (x `elem` y)

happens' :: Eq a => Bool -> SPred a
happens' x
    | x = happens
    | otherwise = doesNotHappen

runQueries :: (Eq a) => Network a -> [B.Query a Bool] -> [B.Probability]
runQueries n qs = map (runQuery n) qs

runQuery :: (Eq a) => Network a -> B.Query a Bool -> B.Probability
runQuery n q
    | length q.conditioned /= 1 = error "Expected exactly one conditioned var for a query on the hackage version of a bNet."
    | otherwise = fromRational $ runQuery' n (head $ M.assocs q.conditioned, M.assocs q.conditional)

runQuery' :: (Eq a) => Network a -> ((a, Bool), [(a, Bool)]) -> P.Probability
runQuery' n ((x, xVal), ys) = happens' xVal x ?? foldr (\(y, yVal) acc -> happens' yVal y ?=<< acc) n ys

{- | A probability potential.

Probability values are only entered for the half of the table
that considers the conditioned variable is true. The probability
values are ordered from False to True, i.e.

X   |   A   B   |   P
-----------------------
T   |   F   F   |   0
T   |   F   T   |   1
T   |   T   F   |   2
T   |   T   T   |   3


X is the conditioned variable, A and B are the conditional variables
and P is probability. A is before B inside the conditional variables
list, and probability values inside the the table are the index of their
position inside the probabilities list.
-}
potential :: (Eq a) => a -> [a] -> [P.Probability] -> Potential a
potential conditionedV conditionalVs ps s = event p conditionedV s
    where
        (p, _) = findAssertSingleMatch snd events

        events :: [(P.Probability, Bool)]
        events = zipAssert ps (map (hasVariableSetOccured s) $ vPermutations conditionalVs)

hasVariableSetOccured :: Eq a => State a -> [(a, Bool)] -> Bool
hasVariableSetOccured s xs = all f xs
    where
        f (var, varIsTrue)
            | varIsTrue = elem var s
            | otherwise = not (elem var s)

vPermutations :: [a] -> [[(a, Bool)]]
vPermutations [] = [[]]
vPermutations (v : vs) = [(v, value) : rest | value <- [False, True], rest <- vPermutations vs]

