{-# LANGUAGE ScopedTypeVariables #-}

-- GHC thinks HasCallStack constraints are redundant.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module LocalComputation.Utils
    ( setMap
    , nubWithBy
    , snd4
    , thd4
    , fth4
    , findAssertSingleMatch
    , unsafeFind
    , unionUnsafe
    , fromListAssertDisjoint
    , unionAssertDisjoint
    , unionAssertDisjoint'
    , unzipWith
    , zipWithAssert
    , zipAssert
    , divAssert
    , toBinary
    , unitTest
    , checkAnswers
    , toBinaryLeadingZeroes
    , assert'
    , integerLogBase2
    , listOfPowersOfTwo
    , safeHead
    , fromListAssertDisjoint'
    , parseFile
    , fromRight
    )
where


import           Data.List                     (find)
import           Data.Map                      (Map, adjust, elems, insert,
                                                member)
import qualified Data.Map                      as M
import           Data.Set                      (Set)
import qualified Data.Set                      as S

import           Data.Functor                  (void)
import           Hedgehog                      (Property, PropertyT, diff,
                                                property, withTests)
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

unsafeFind :: Foldable t => (a -> Bool) -> t a -> a
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

assert' :: HasCallStack => (a -> Bool) -> a -> a
assert' p x = assert (p x) x

unitTest :: PropertyT IO a -> Property
unitTest = withTests 1 . property . void

checkAnswers :: (Show a) => (a -> a -> Bool) -> [a] -> [a] -> PropertyT IO ()
checkAnswers f results answers = diff results (\rs as -> and (zipWithAssert f rs as)) answers

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

parseFile :: P.GenParser Char () a -> FilePath -> IO (Either P.ParseError a)
parseFile p filename = do
    handle <- openFile filename ReadMode
    contents <- hGetContents' handle
    pure $ P.parse p filename contents

fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight _         = error "fromRight received a Left"

