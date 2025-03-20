{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Algebra.Graph
import Control.Concurrent (threadDelay)
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Data.Binary
import Data.List (nub)
import Data.Typeable
import Lib
import Network.Transport.TCP

type Domain a = [a]

domainIntersects :: (Eq a) => Domain a -> Domain a -> Bool
domainIntersects xs ys = or [x == y | x <- xs, y <- ys]

-- I think there is a way to add quick tests or something to ensure
-- certain mathematical properties hold.

-- Each instance of valuation will have a unique 'a' (valuation) AND 'b' (variable)
-- which is reflected by the functional dependency 'a -> b'.
--
-- Eq b was added for "primalGraph"
class (Eq b) => Valuation a b | a -> b where
  label :: a -> Domain b
  combine :: a -> a -> a
  project :: a -> Domain b -> a

data ValNode a = ValNode {vertexNum :: Integer, contents :: a} deriving (Show)

instance Eq (ValNode a) where
  (==) (ValNode {vertexNum = x}) (ValNode {vertexNum = y}) = x == y

instance Ord (ValNode a) where
  (<=) (ValNode {vertexNum = x}) (ValNode {vertexNum = y}) = x <= y

---------------- BAYESIAN

-- This implementation only handles binary events.

{-
In BValColumns, the first parameter is the variable, the second parameter
is a list of the conditions, and the third parameter is the list of probabilities,
where probabilities are ordered as follows:

    var   A   B   Probability

     0    0   0       0.7       P(var == 0 | A == 0 && B == 0)

     0    0   1       0.4       P(var == 0 | A == 0 && B == 1)

     0    0   2       0.3       P(var == 0 | A == 0 && B == 2)

     0    1   0       0.6       P(var == 0 | A == 1 && B == 0)

BValRows stores the equivalent information in a different format, storing each
row of the table instead.

BValColumns stores no redundant information, while BValRows stores a heap of redundant information,
but allows accessing this information in a more haskell-like manner.
-}
data BVal a b = BValRows [BValRow a b]

-- An inefficent storage format, but we should get a working implementation first.
data BValRow a b = BValRow
  { variable :: (a, b),
    conditions :: [(a, b)],
    probability :: Probability
  }
  deriving (Show)

-- A supporting data structure, as inputting data in this format is often easier.
data BValColumns a b = BValColumns a [a] [Probability] | BValColumnsNull deriving (Show)

getColumns :: BVal a b -> BValColumns a b
getColumns (BValRows []) = BValColumnsNull
getColumns (BValRows rs'@(r : _)) = BValColumns v cs ps
  where
    v = fst (variable r)
    cs = map (fst) (conditions r)
    ps = map probability rs'

getRows :: forall a b. (Enum b, Bounded b) => BValColumns a b -> BVal a b
getRows BValColumnsNull = BValRows []
getRows (BValColumns var conds ps) = BValRows fullRows'
  where
    fullRows' :: [BValRow a b]
    fullRows' = map (\(x, y, z) -> BValRow x y z) fullRows

    fullRows :: [((a, b), [(a, b)], Probability)]
    fullRows = zipWith (\(v, cs) p -> (v, cs, p)) rowsWithoutProbability ps

    rowsWithoutProbability :: [((a, b), [(a, b)])]
    rowsWithoutProbability = [(v, cs) | v <- vPermutations, cs <- csPermutations conds]
      where
        vPermutations :: [(a, b)]
        vPermutations = [(var, vVal) | vVal <- varValues]

        csPermutations :: [a] -> [[(a, b)]]
        csPermutations [] = [[]]
        csPermutations (c : cs) = [(c, cVal) : rest | cVal <- varValues, rest <- csPermutations cs]

        varValues :: [b]
        varValues = [minBound .. maxBound]

type Probability = Float

-- Don't be suprised if you need to put (Enum, bounded) on 'b'.
instance (Eq a) => Valuation (BVal a b) a where
  -- label :: BVal -> Domain BVar
  label (BValRows []) = []
  label (BValRows (x : _)) = fst (variable x) : map fst (conditions x)

  -- combine :: BVal -> BVal -> BVal
  combine = undefined

  -- There is a lot about the data format i'm unsure about here -
  -- what if we get a p1 = A | B C and p2 = B | C scenario? Can this happen?

  -- project :: BVal -> Domain BVar -> BVal
  project = undefined

instance (Show a) => Show (BVal a b) where
  show xs = show $ getColumns xs

---------------- BAYESIAN END

---------------- BAYESIAN PROBLEM EXAMPLE
data P1Var = F | B | L | D | H deriving (Eq, Ord, Show)

data P1Value = P1False | P1True deriving (Enum, Bounded, Show)

p1Valuations :: [BVal P1Var P1Value]
p1Valuations =
  [ getRows $ BValColumns F [] [0.85, 0.15],
    getRows $ BValColumns B [] [0.99, 0.01],
    getRows $ BValColumns L [F] [0.95, 0.4, 0.05, 0.6],
    getRows $ BValColumns D [F, B] [0.7, 0.03, 0.1, 0.01, 0.3, 0.97, 0.9, 0.99],
    getRows $ BValColumns H [D] [0.99, 0.3, 0.01, 0.7]
  ]

-- BAYESIAN PROBLEM EXAMPLE END

----------------- JOIN TREE CONSTRUCTION

primalGraph :: (Valuation a b) => [a] -> Graph (ValNode a)
primalGraph bs = primalGraph' $ zip (zipWith (\x y -> vertex $ ValNode x y) [1 ..] bs) (fmap label bs)
  where
    primalGraph' :: (Valuation a b) => [(Graph (ValNode a), Domain b)] -> Graph (ValNode a)
    primalGraph' [] = empty
    primalGraph' ((v, d) : xs) = overlay connectionsToHead (primalGraph' xs)
      where
        connectionsToHead = (overlays [overlay (connect v v') (connect v' v) | (v', d') <- xs, domainIntersects d d'])

{-

Construct the tree by triangulation and then assign numbers and edge directions?
Q: What happens when a node holding P(A | B) points to one holding P(A)?
A: When P(A | B) tries to combine I believe it first marginalizes to P(A)'s domain, so the
   B is removed, and we probably get an identity combination.

-}

-- joinTree :: Graph (ValNode a) -> Graph (ValNode a)
-- joinTree graph = foldr f graph (vertexList graph)
--   where
--     f v acc = undefined

neighbours :: Graph a -> a -> [a]
neighbours graph v = undefined

--

----------------- COLLECT ALGORITHM

-- COLLECT ALGORITHM END

node :: ReceivePort (a, SendPort a)
node = undefined

main :: IO ()
main = do
  print "foo"
