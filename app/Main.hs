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
import Data.List (intersperse, nub, union)
import Data.Tree
import Data.Typeable
import Debug.Trace
import Lib
import Network.Transport.TCP

----------
-- utils
------------

setDifference :: (Eq a) => [a] -> [a] -> [a]
setDifference xs ys = filter (\x -> not $ x `elem` ys) xs

-- Consider creating a typeclass for variables that has them implement 'hashCode' - nenok uses
-- this to implement the binary heap ordering?

type Domain a = [a]

domainIntersects :: (Eq a) => Domain a -> Domain a -> Bool
domainIntersects xs ys = or [x == y | x <- xs, y <- ys]

-- I think there is a way to add quick tests or something to ensure
-- certain mathematical properties hold.

-- Each instance of valuation will have a unique 'a' (valuation) AND 'b' (variable)
-- which is reflected by the functional dependency 'a -> b'.
--
-- Eq b was added for "primalGraph"
-- The a -> b functional dependency is valid here, because 'a' will be of some form
-- 'ValInstance d b'. Hence, by picking our 'a', we have picked our 'b'; so the "a -> b"
-- functional dependency would hinder our implementation.
class Valuation val where
  label :: val var -> Domain var
  combine :: val var -> val var -> val var
  project :: val var -> Domain var -> val var

-- Notably when we want class constraints, we now place them around the functions instead of at the typeclass level.
-- For example, I wouldn't be suprised if in 'collect' we had to draw on the knowledge that 'val' is a 'Valuation',
-- so we could add the class constraint '(Valuation val) => ...' to the front of the 'collect' method.
class JoinTreeNode node where
  collect :: node val var -> node val var
  getValuation :: node val var -> Maybe (val var)
  getDomain :: node val var -> Domain var
  create :: Integer -> Domain var -> Maybe (val var) -> node val var
  nodeId :: node val var -> Integer

data CollectNode val var = CollectNode Integer (Domain var) (Maybe (val var))

instance (Valuation val) => Eq (CollectNode val var) where
  (==) x y = nodeId x == nodeId y

instance (Valuation val) => Ord (CollectNode val var) where
  (<=) x y = nodeId x <= nodeId y

instance JoinTreeNode CollectNode where
  collect = undefined
  getValuation (CollectNode _ _ v) = v
  getDomain (CollectNode _ d _) = d
  create = CollectNode
  nodeId (CollectNode i _ _) = i

instance (Eq var, Show var) => Show (CollectNode (BVal varValue) var) where
  show (CollectNode x y (Just z)) = show x ++ " - " ++ show y ++ " - " ++ (show $ getColumns z)
  show (CollectNode x y Nothing) = show x ++ " - " ++ show y ++ " - " ++ "Nothing"

-- instance (Valuation val var, Show val, Show var) => Show (CollectNode val var) where
--   show (CollectNode x y _) = show x ++ " - " ++ show y

-- Heres the problem - say there are two instances of JoinTreeNode,
-- 'CollectNode' and 'ShenoyNode'. Now, in a function you call
--
-- `domain myNode :: Domain BayesianVariable`
--
-- here, haskell doesn't know whether to use the 'CollectNode' or
-- 'ShenoyNode' to evaluate the domain method - and we don't want our
-- function to tell haskell whether or not it's a 'ShenoyNode' or 'CollectNode'
-- because the whole point is to have our function accept ANY node.

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
data BVal var val = BValRows [BValRow val var]

-- An inefficent storage format, but we should get a working implementation first.
data BValRow a b = BValRow
  { variable :: (a, b),
    conditions :: [(a, b)],
    probability :: Probability
  }
  deriving (Show)

-- A supporting data structure, as inputting data in this format is often easier.
data BValColumns a b = BValColumns a [a] [Probability] | BValColumnsNull deriving (Show)

getColumns :: BVal var val -> BValColumns val var
getColumns (BValRows []) = BValColumnsNull
getColumns (BValRows rs'@(r : _)) = BValColumns v cs ps
  where
    v = fst (variable r)
    cs = map (fst) (conditions r)
    ps = map probability rs'

getRows :: forall val var. (Enum var, Bounded var) => BValColumns val var -> BVal var val
getRows BValColumnsNull = BValRows []
getRows (BValColumns var conds ps) = BValRows fullRows'
  where
    fullRows' :: [BValRow val var]
    fullRows' = map (\(x, y, z) -> BValRow x y z) fullRows

    fullRows :: [((val, var), [(val, var)], Probability)]
    fullRows = zipWith (\(v, cs) p -> (v, cs, p)) rowsWithoutProbability ps

    rowsWithoutProbability :: [((val, var), [(val, var)])]
    rowsWithoutProbability = [(v, cs) | v <- vPermutations, cs <- csPermutations conds]
      where
        vPermutations :: [(val, var)]
        vPermutations = [(var, vVal) | vVal <- varValues]

        csPermutations :: [val] -> [[(val, var)]]
        csPermutations [] = [[]]
        csPermutations (c : cs) = [(c, cVal) : rest | cVal <- varValues, rest <- csPermutations cs]

        varValues :: [var]
        varValues = [minBound .. maxBound]

type Probability = Float

-- Don't be suprised if you need to put (Enum, bounded) on 'b'.
instance Valuation (BVal varVal) where
  -- label :: BVal -> Domain BVar
  label (BValRows []) = []
  label (BValRows (x : _)) = fst (variable x) : map fst (conditions x)

  -- combine :: BVal -> BVal -> BVal
  combine = undefined

  -- There is a lot about the data format i'm unsure about here -
  -- what if we get a p1 = A | B C and p2 = B | C scenario? Can this happen?

  -- project :: BVal -> Domain BVar -> BVal
  project = undefined

instance (Show b) => Show (BVal a b) where
  show xs = show $ getColumns xs

---------------- BAYESIAN END

---------------- BAYESIAN PROBLEM EXAMPLE
data P1Var = F | B | L | D | H deriving (Eq, Ord, Show)

data P1Value = P1False | P1True deriving (Enum, Bounded, Show)

p1Valuations :: [BVal P1Value P1Var]
p1Valuations =
  [ getRows $ BValColumns F [] [0.85, 0.15],
    getRows $ BValColumns B [] [0.99, 0.01],
    getRows $ BValColumns L [F] [0.95, 0.4, 0.05, 0.6],
    getRows $ BValColumns D [F, B] [0.7, 0.03, 0.1, 0.01, 0.3, 0.97, 0.9, 0.99],
    getRows $ BValColumns H [D] [0.99, 0.3, 0.01, 0.7]
  ]

-- BAYESIAN PROBLEM EXAMPLE END

----------------- JOIN TREE CONSTRUCTION

-- Another possible implementation:
-- data JoinTreeNode = CollectNode [function that collects for collect node] | OtherNode [...]
--
-- I think this does technically loosen typing rules:
--  A tree that has mixed nodes would type correctly.

-- primalGraph :: (Valuation a b) => [a] -> Graph (ValNode a)
-- primalGraph bs = primalGraph' $ zip (zipWith (\x y -> vertex $ ValNode x y) [1 ..] bs) (fmap label bs)
--   where
--     primalGraph' :: (Valuation a b) => [(Graph (ValNode a), Domain b)] -> Graph (ValNode a)
--     primalGraph' [] = empty
--     primalGraph' ((v, d) : xs) = overlay connectionsToHead (primalGraph' xs)
--       where
--         connectionsToHead = (overlays [overlay (connect v v') (connect v' v) | (v', d') <- xs, domainIntersects d d'])

triangulatedGraph :: Graph (ValNode a) -> Graph (ValNode a)
triangulatedGraph = undefined

showAdjacents :: (Ord a, Show a) => (Graph a) -> String
showAdjacents graph = concat $ intersperse "\n\n\n" $ fmap (show) (adjacencyList graph)

showAdjacentsValNode :: Graph (ValNode a) -> String
showAdjacentsValNode graph = concat $ intersperse "\n\n" $ fmap showVertices (adjacencyList graph)
  where
    showVertices :: (ValNode a, [ValNode a]) -> String
    showVertices (x, ys) = show (vertexNum x) ++ " -> " ++ (show $ map (\y -> vertexNum y) ys)

joinTree :: forall node val var. (JoinTreeNode node, Valuation val, Eq var, Eq (node val var)) => [val var] -> Graph (node val var)
joinTree vs = edges $ joinTree' nextNodeId r d
  where
    d :: Domain var
    d = foldr union [] $ map label vs

    r :: [node val var]
    r = zipWith (\nid v -> create nid (label v) (Just v)) [0 ..] vs

    nextNodeId :: Integer
    nextNodeId = fromIntegral $ length r

joinTree' :: forall node val var. (JoinTreeNode node, Valuation val, Eq var, Eq (node val var)) => Integer -> [node val var] -> Domain var -> [(node val var, node val var)]
joinTree' _ _ [] = []
joinTree' nextNodeId r (x : d')
  | length r <= 1 = []
  | length r' > 0 = union (union [(nUnion, nP)] e) (joinTree' (nextNodeId + 2) (union [nP] r') d')
  | otherwise = union e (joinTree' (nextNodeId + 2) r' d')
  where
    xIsInNodeDomain :: node val var -> Bool
    xIsInNodeDomain n = x `elem` (getDomain n)

    phiX :: [node val var]
    phiX = filter xIsInNodeDomain r

    domainOfPhiX :: Domain var
    domainOfPhiX = foldr union [] $ map getDomain phiX

    nUnion :: node val var
    nUnion = create nextNodeId domainOfPhiX Nothing

    r' :: [node val var]
    r' = setDifference r phiX

    e :: [(node val var, node val var)]
    e = [(n, nUnion) | n <- phiX]

    nP :: node val var
    nP = create (nextNodeId + 1) (setDifference domainOfPhiX [x]) Nothing

-- Just because every node has a valuation at the start doesn't mean that every node must have a valuation.
-- I'm not exactly sure what it means to have a node that does not have a valuation on it when it comes
-- to the time for message passing, but I believe i have seen similar things in my readings.

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

p1JoinTree :: Graph (CollectNode (BVal P1Value) P1Var)
p1JoinTree = joinTree p1Valuations

main :: IO ()
main = do
  -- putStrLn $ showAdjacentsValNode (primalGraph p1Valuations)
  putStrLn $ showAdjacents p1JoinTree
  print $ map label p1Valuations
