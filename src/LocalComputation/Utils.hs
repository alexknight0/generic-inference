{-# LANGUAGE ScopedTypeVariables #-}

-- GHC thinks HasCallStack constraints are redundant.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module LocalComputation.Utils
    ( setMap
    , nubWithBy
    , uniqueOn
    , snd4
    , thd4
    , fth4
    , findAssertSingleMatch
    , unsafeFind
    , unionUnsafe
    , fromListAssertDisjoint
    , fromListA
    , unionAssertDisjoint
    , unionAssertDisjoint'
    , unzipWith
    , zipWithAssert
    , zipAssert
    , divAssert
    , toBinary
    , toBinaryLeadingZeroes
    , assert'
    , assertP
    , integerLogBase2
    , listOfPowersOfTwo
    , safeHead
    , fromListAssertDisjoint'
    , parseFile
    , parseFile'
    , fromRight
    , lookupDefault
    , lookupDefaultR
    , fromList''
    , formatTimeNicely
    , infinity
    , neighbours
    , unusedArg
    , Table (Table, heading, rows)
    , assertError
    )
where


import qualified Algebra.Graph                 as DG
import qualified Data.Bimap                    as BM
import           Data.List                     (find)
import           Data.Map                      (Map, adjust, elems, insert,
                                                member)
import qualified Data.Map                      as M
import           Data.Set                      (Set)
import qualified Data.Set                      as S
import           Data.Time                     (UTCTime, utctDayTime)
import           Text.Printf                   (printf)

import qualified Text.ParserCombinators.Parsec as P

import           Control.Exception             (assert)
import           GHC.Stack                     (HasCallStack)
import           Numeric.Natural
import           System.IO                     (IOMode (ReadMode),
                                                hGetContents', openFile)

divAssert :: (Integral a) => a -> a -> a
divAssert x y
    | x `mod` y /= 0 = error "Was remainder after division"
    | otherwise = x `div` y

setMap :: (Ord b) => (a -> b) -> Set a -> Set b
setMap f xs = S.map f xs

nubWithBy :: forall a b . (Ord b) => (a -> b) -> (a -> a -> a) -> [a] -> [a]
nubWithBy toKey f xs = elems $ foldr g M.empty xs
    where
        g :: a -> Map b a -> Map b a
        g x acc
            | toKey x `member` acc = adjust (\y -> f x y) (toKey x) acc
            | otherwise = insert (toKey x) x acc

uniqueOn :: (Ord b) => (a -> b) -> [a] -> Bool
uniqueOn f xs = length xs == (length $ S.fromList $ map f xs)

thd4 :: (a, b, c, d) -> c
thd4 (_, _, x, _) = x

snd4 :: (a, b, c, d) -> b
snd4 (_, x, _, _) = x

fth4 :: (a, b, c, d) -> d
fth4 (_, _, _, x) = x

findAssertSingleMatch :: (a -> Bool) -> [a] -> a
findAssertSingleMatch p xs
    | [y] <- filter p xs = y
    | ys <- filter p xs = let numMatches = length (take 10000 ys) in
                                error $ "findAssertSingleMatch found " ++ (if numMatches == 10000 then ">" else "") ++ show numMatches ++ " matches"

unsafeFind :: HasCallStack => Foldable t => (a -> Bool) -> t a -> a
unsafeFind p xs
    | (Just y) <- Data.List.find p xs = y
    | otherwise = error "unsafeFind found nothing"

unionUnsafe :: (Eq b, Ord a) => Map a b -> Map a b -> Map a b
unionUnsafe = M.unionWith (\x y -> if x /= y then error "Map keys that exist in both maps do not have the same value" else x)

unionAssertDisjoint :: (HasCallStack, Ord a) => Map a b -> Map a b -> Map a b
unionAssertDisjoint = M.unionWith (\_ _ -> error "Map key sets are not disjoint")

unionAssertDisjoint' :: (HasCallStack, Ord a) => Set a -> Set a -> Set a
unionAssertDisjoint' x y
    | not (S.disjoint x y) = error "Sets are not disjoint."
    | otherwise = S.union x y

fromListAssertDisjoint :: (Ord a) => [(a, b)] -> Map a b
fromListAssertDisjoint = M.fromListWith (\_ _ -> error "Attempted to create map from non disjoint assoc list")

fromListA :: (Ord a) => [(a, b)] -> Map a b
fromListA = fromListAssertDisjoint

fromListAssertDisjoint' :: Ord a => [a] -> Set a
fromListAssertDisjoint' xs = assert' (\ys -> length ys == length xs) (S.fromList xs)

unzipWith :: (a -> (b, c)) -> [a] -> ([b], [c])
unzipWith f xs = (map (fst . f) xs, map (snd . f) xs)

zipWithAssert :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWithAssert f xs ys
    | length xs /= length ys = error "Length of lists didn't match"
    | otherwise = zipWith f xs ys

zipAssert :: [a] -> [b] -> [(a,b)]
zipAssert xs ys
    | length xs /= length ys = error "Length of lists didn't match"
    | otherwise = zip xs ys

-- | Returns result with MSB as the head of the list.
toBinary :: Natural -> [Bool]
toBinary x = toBinary' x []
    where
        toBinary' :: Natural -> [Natural] -> [Bool]
        toBinary' 0 digits = map (toEnum . fromIntegral) digits
        toBinary' n digits
            | otherwise = toBinary' (n `quot` 2) (n `mod` 2 : digits)

toBinaryLeadingZeroes :: Natural -> Natural -> [Bool]
toBinaryLeadingZeroes totalDigits x = take numLeadingZeroes (repeat False) ++ binary
    where
        binary = toBinary x
        numLeadingZeroes = assert' (>=0) (fromIntegral totalDigits - length binary)

-- TODO: Remove all calls and replace with assertId
assert' :: HasCallStack => (a -> Bool) -> a -> a
assert' p x = assert (p x) x

-- | Asserts a given predicate holds for a given value.
assertP :: HasCallStack => (a -> Bool) -> a -> a
assertP p x = assert (p x) x

integerLogBase2 :: Natural -> Maybe Natural
integerLogBase2 x = integerLogBase2' 0 x
    where
        integerLogBase2' :: Natural -> Natural -> Maybe Natural
        integerLogBase2' _ 0 = Nothing
        integerLogBase2' acc 1 = Just acc
        integerLogBase2' acc y
            | y `mod` 2 == 0 = integerLogBase2' (acc + 1) (y `div` 2)
            | otherwise = Nothing

listOfPowersOfTwo :: [Int]
listOfPowersOfTwo = 1 : (map (*2) listOfPowersOfTwo)

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

-- TODO: Move to benchmark utils file.
parseFile :: P.GenParser Char () a -> FilePath -> IO (Either P.ParseError a)
parseFile p filename = do
    handle <- openFile filename ReadMode
    contents <- hGetContents' handle
    pure $ P.parse p filename contents

parseFile' :: P.GenParser Char () a -> FilePath -> IO a
parseFile' p filename = do
    handle <- openFile filename ReadMode
    contents <- hGetContents' handle
    pure $ fromRight $ P.parse p filename contents

fromRight :: HasCallStack => Either a b -> b
fromRight (Right x) = x
fromRight _         = error "fromRight received a Left"

lookupDefault :: (Ord a, Ord b) => a -> b -> BM.Bimap a b -> b
lookupDefault x defaultElem m
    | BM.member x m = (BM.!) m x
    | otherwise = defaultElem

lookupDefaultR :: (Ord a, Ord b) => b -> a -> BM.Bimap a b -> a
lookupDefaultR x defaultElem m
    | BM.memberR x m = (BM.!>) m x
    | otherwise = defaultElem

-- | \(O(n \log n)\) - Creates a map from the given association list,
-- returning `Nothing` if there a duplicate keys in the list.
fromList'' :: (Ord a) => [(a, b)] -> Maybe (M.Map a b)
fromList'' = foldr f (Just M.empty)
    where
        f (_, _) Nothing = Nothing
        f (k, v) (Just acc)
            | M.member k acc = Nothing
            | otherwise = Just $ M.insert k v acc


formatTimeNicely :: UTCTime -> String
formatTimeNicely time = printf "[%02d:%02d:%02d]" hours minutes seconds

    where
        secondsPastMidnight :: Integer
        secondsPastMidnight = floor $ utctDayTime time

        seconds = secondsPastMidnight `mod` 60
        minutes = secondsPastMidnight `div` 60 `mod` 60
        hours = secondsPastMidnight `div` 60 `div` 60 `mod` 60

infinity :: Double
infinity = read "Infinity"

neighbours :: Ord a  => a -> DG.Graph a -> Maybe [a]
neighbours x g = do
    (_, adjacent) <- find (\(y, _) -> y == x) $ DG.adjacencyList g
    pure adjacent

unusedArg :: a
unusedArg = error "Argument should not be used"

-- TODO: Implicit assumption that length of heading equals length of rows. Could assert this.
data Table = Table {
      heading :: [String]
    , rows    :: [[String]]
}

assertError :: a
assertError = assert False undefined

