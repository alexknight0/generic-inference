{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LocalComputation.LabelledMatrix
    ( LabelledMatrix
    , fromMatrix
    , fromList
    , fromListDefault
    , domain
    , isSquare
    , toSquare
    , squareDomain
    , identity
    , extension
    , project
    , find
    , add
    , multiply
    , multiplys
    , quasiInverse
    , decompose
    , joinSquare
    , isWellFormed
    , isSymmetric
    )
where

import           Control.Exception                                            (assert)
import qualified Control.Monad                                                as Monad
import qualified Data.Map                                                     as M
import qualified Data.Matrix                                                  as M'
import           Data.Maybe                                                   (fromJust,
                                                                               isJust)
import qualified Data.Set                                                     as S
import           LocalComputation.Utils
import qualified LocalComputation.ValuationAlgebra.QuasiRegular.SemiringValue as Q

-- Typeclasses
import           Control.DeepSeq                                              (NFData)
import           Data.Binary                                                  (Binary)
import           GHC.Generics                                                 (Generic)


data InvalidFormat = DuplicateKeys | NotTotalMapping

{- | A labelled matrix.

A column or row matrix can be created by specifying `LabelledMatrix () b c` or `LabelledMatrix a () c`
-}
data LabelledMatrix a b c = Matrix (M.Map (a, b) c) (S.Set a) (S.Set b) deriving (Binary, NFData, Ord, Generic, Read)

instance (Eq a, Eq b, Eq c) => Eq (LabelledMatrix a b c) where
    (Matrix m1 _ _) == (Matrix m2 _ _) = m1 == m2

instance (Show a, Show b, Show c) => Show (LabelledMatrix a b c) where
    show (Matrix m _ _) = show m

instance Functor (LabelledMatrix a b) where
    fmap :: (c -> d) -> LabelledMatrix a b c -> LabelledMatrix a b d
    fmap f (Matrix m dA dB) = Matrix (M.map f m) dA dB

-- | Transforms a regular matrix from Data.Matrix into a matrix labelled by Integers.
fromMatrix :: M'.Matrix a -> LabelledMatrix Integer Integer a
fromMatrix m = fromRight $ fromList $ concat $ zipWith (\i row -> zipWith (\j x -> ((i, j), x)) [0..] row) [0..] (M'.toLists m)

-- | Creates a matrix from the given association list.
fromList :: forall a b c . (Ord a, Ord b)
    => [((a, b), c)]
    -> Either InvalidFormat (LabelledMatrix a b c)
fromList xs = fromList' xs Nothing

-- | Creates a matrix from the given association list. Doesn't require the input list to be a total mapping - will
-- fill unset values with the given default element.
fromListDefault :: forall a b c . (Ord a, Ord b)
    => [((a, b), c)]
    -> c
    -> Either InvalidFormat (LabelledMatrix a b c)
fromListDefault xs default' = fromList' xs (Just default')

-- | Internal function used to create a list with an optional default element.
fromList' :: forall a b c . (Ord a, Ord b)
    => [((a, b), c)]
    -> Maybe c
    -> Either InvalidFormat (LabelledMatrix a b c)
fromList' xs defaultElem
    | length xs /= (length (nubWithBy fst const xs))             = Left DuplicateKeys
    | Nothing <- defaultElem, length as * length bs /= length xs = Left NotTotalMapping
    | otherwise                                                  = Right $ Matrix (M.union (mapFromList xs) mapWithAllDefault) as bs
    where
        defaultElem'
            | Just x <- defaultElem = x
            | otherwise = error "Internal error: should never evaluate as \
                                \default element should never be used."
        mapFromList = M.fromListWith (\_ _ -> error "Internal error: duplicate key in matrix creation \
                                                    \association list despite earlier check.")

        as = S.fromList $ map (\((x, _), _) -> x) xs
        bs = S.fromList $ map (\((_, x), _) -> x) xs
        mapWithAllDefault = M.fromList [((a, b), defaultElem') | a <- S.toList as, b <- S.toList bs]

isSymmetric :: (Ord a, Eq c) => LabelledMatrix a a c -> Bool
isSymmetric x | assertIsWellFormed x = undefined
isSymmetric (Matrix m _ _) = all (\((x, y), v) -> m M.! (y, x) == v) (M.toList m)

isSquare :: LabelledMatrix a b c -> Bool
isSquare x | assertIsWellFormed x = undefined
isSquare (Matrix _ dA dB) = length dA == length dB

domain :: LabelledMatrix a b c -> (S.Set a, S.Set b)
domain (Matrix _ dA dB) = (dA, dB)

toSquare :: (Ord a) => LabelledMatrix a a c -> c -> LabelledMatrix a a c
toSquare x _ | assertIsWellFormed x = undefined
toSquare x@(Matrix _ dA1 dA2) zero = fromJust $ extension x d d zero
    where
        d = S.union dA1 dA2

-- | Returns the simplified domain of a square matrix that has the same domain for both labels. Returns Nothing if the given matrix is not square.
squareDomain :: (Eq a) => LabelledMatrix a a c -> Maybe (S.Set a)
squareDomain x | assertIsWellFormed x = undefined
squareDomain (Matrix _ dA1 dA2)
    | dA1 /= dA2 = Nothing
    | otherwise = Just dA1

-- | Returns the identity matrix created with the given zero and one elements.
identity :: (Ord a) => S.Set a -> c -> c -> LabelledMatrix a a c
identity dA zero one = fromRight $ fromList [((x, y), if x == y then one else zero) | x <- as, y <- as]
    where
        as = S.toList dA

-- | Extend a matrix to a larger domain, filling spots with the given zero element. Returns Nothing if the domain to extend to is not a superset.
extension :: (Ord a, Ord b) => LabelledMatrix a b c -> S.Set a -> S.Set b -> c -> Maybe (LabelledMatrix a b c)
extension x _ _ _ | assertIsWellFormed x = undefined
extension (Matrix m dA dB) newDA newDB zero
    | dA `S.isSubsetOf` newDA && dB `S.isSubsetOf` newDB = Just $ Matrix (m `M.union` mapOfZeroes) newDA newDB
    | otherwise                                          = Nothing
    where
        mapOfZeroes = M.fromList [((a, b), zero) | a <- S.toList newDA, b <- S.toList newDB]

-- | Project the domain of a matrix down to a new domain. Returns nothing if the given domain is not a subset of the old domain.
project :: (Ord a, Ord b) => LabelledMatrix a b c -> S.Set a -> S.Set b -> Maybe (LabelledMatrix a b c)
project x _ _ | assertIsWellFormed x = undefined
project (Matrix m dA dB) newDA newDB
    | newDA `S.isSubsetOf` dA && newDB `S.isSubsetOf` dB = Just $ Matrix (M.filterWithKey (\(a, b) _ -> a `elem` newDA && b `elem` newDB) m) newDA newDB
    | otherwise = Nothing

-- | Returns an element from the matrix. Returns Nothing if the element is not in the domain of the matrix.
find :: (Ord a, Ord b) => (a, b) -> LabelledMatrix a b c -> Maybe c
find _ x | assertIsWellFormed x = undefined
find (a, b) (Matrix m dA dB)
    | a `elem` dA && b `elem` dB = assert' isJust $ M.lookup (a, b) m
    | otherwise = Nothing

-- | Basic addition on two matrices. Returns Nothing if the provided matrices have different shapes.
add :: (Ord a, Ord b) => (c -> c -> c) -> LabelledMatrix a b c -> LabelledMatrix a b c -> Maybe (LabelledMatrix a b c)
add _ x y | assertAllWellFormed [x, y] = undefined
add addElems (Matrix m1 dA1 dB1) (Matrix m2 dA2 dB2)
    | dA1 /= dA2 || dB1 /= dB2 = Nothing
    | otherwise = Just $ Matrix (M.intersectionWith addElems m1 m2) dA1 dB1

-- | Basic matrix multiplication on two matrices. Returns Nothing if the provided matrices have the wrong shape for matrix multiplication.
multiply :: (Ord a, Ord b, Ord c) => (d -> d -> d) -> (d -> d -> d) -> d -> LabelledMatrix a b d -> LabelledMatrix b c d -> Maybe (LabelledMatrix a c d)
multiply _ _ _ x y | assertIsWellFormed x || assertIsWellFormed y = undefined
multiply addElems multiplyElems zero m1@(Matrix _ dA dB1) m2@(Matrix _ dB2 dC)
    | dB1 /= dB2 = Nothing
    | otherwise = Just $ Matrix (fromListAssertDisjoint newAssocList) dA dC
    where
        (as, bs, cs) = (S.toList dA, S.toList dB1, S.toList dC)

        newAssocList = [(  (a, c),
                           foldr addElems zero [fromJust (find (a, b) m1) `multiplyElems` fromJust (find (b, c) m2) | b <- bs]
                        ) | a <- as, c <- cs]

multiplys :: (Functor t, Foldable t, Ord a) => (c -> c -> c) -> (c -> c -> c) -> c -> t (LabelledMatrix a a c) -> Maybe (LabelledMatrix a a c)
multiplys _ _ _ xs | assertAllWellFormed xs = undefined
multiplys addElems multiplyElems zero xs
    | null xs = Nothing
    | otherwise = foldl1 (liftA2' (multiply addElems multiplyElems zero)) (fmap Just xs)
    where
        liftA2' f x y = Monad.join (liftA2 f x y)

{- | Computes the quasi-inverse of a given matrix, returning Nothing if the given matrix is not square.

This formula is detailed in "Generic Inference" (Pouly and Kohlas, 2012).
-}
quasiInverse :: (Ord a, Q.QuasiRegularSemiringValue c, Show a, Show c) => LabelledMatrix a a c -> Maybe (LabelledMatrix a a c)
quasiInverse x | assertIsWellFormed x = undefined
quasiInverse m@(Matrix _ dA dB)
    | length dA /= length dB = Nothing
    | length dA == 0 = Just $ m
    | length dA == 1 = Just $ fmap Q.quasiInverse m
    | otherwise = assert' isJust $ joinSquare newB newC newD newE
    where
        (b, c, d, e) = fromJust $ decompose m
        f = add' e (multiplys' [d, bStar, c])
        bStar = quasiInverse' b
        fStar = quasiInverse' f

        newB = add' bStar (multiplys' [bStar, c, fStar, d, bStar])
        newC = multiplys' [bStar, c, fStar]
        newD = multiplys' [fStar, d, bStar]
        newE = fStar

        add' x y = fromJust $ add Q.add x y
        multiplys' xs = fromJust $ multiplys Q.add Q.multiply Q.zero xs
        quasiInverse' x = fromJust $ quasiInverse x

{- Decomposes a matrix into four matrices.

Returns a tuple (A, B, C, D) defined through the following shape:

 ┌─   ─┐
 │ A B │
 │ C D │
 └─   ─┘

Where A is a 1x1 matrix. Returns Nothing if the matrix is empty.
-}
decompose :: (Ord a, Ord b) => LabelledMatrix a b c -> Maybe (LabelledMatrix a b c, LabelledMatrix a b c, LabelledMatrix a b c, LabelledMatrix a b c)
decompose x | assertIsWellFormed x = undefined
decompose m@(Matrix _ dA dB) = do
    aDA <- takeOne dA
    aDB <- takeOne dB

    let dA' = dA `S.difference` aDA
        dB' = dB `S.difference` aDB

    pure (project' m aDA aDB, project' m aDA dB',
          project' m dA' aDB, project' m dA' dB')

    where
        takeOne :: S.Set a -> Maybe (S.Set a)
        takeOne xs = fmap S.singleton $ safeHead $ S.toList xs

        project' x y z = fromJust $ project x y z

-- | Joins two disjoint matrices. Returns Nothing if the matrices are not disjoint. Result may not be well-formed, so should probably not be exposed.
join :: (Ord a, Ord b) =>LabelledMatrix a b c -> LabelledMatrix a b c -> Maybe (LabelledMatrix a b c)
join (Matrix m1 dA1 dB1) (Matrix m2 dA2 dB2)
    | not (M.disjoint m1 m2) = Nothing
    | otherwise = Just $ Matrix (unionDisjoint m1 m2) (S.union dA1 dA2) (S.union dB1 dB2)
    where
        unionDisjoint = M.unionWith (\_ _ -> error "Maps not disjoint despite sets indicating disjoint")

{- Joins four matrices, returning Nothing if the matrices are not arranged in a square.

For inputs A -> B -> C -> D returns:

 ┌─   ─┐
 │ A B │
 │ C D │
 └─   ─┘

Note that this does not indicate that the resulting matrix is a square matrix.
-}
joinSquare :: (Ord a, Ord b) =>LabelledMatrix a b c -> LabelledMatrix a b c -> LabelledMatrix a b c -> LabelledMatrix a b c -> Maybe (LabelledMatrix a b c)
joinSquare a b c d | assertAllWellFormed [a, b, c, d] = undefined
joinSquare mA@(Matrix _ aDA aDB) mB@(Matrix _ bDA bDB) mC@(Matrix _ cDA cDB) mD@(Matrix _ dDA dDB)
    | aDA /= bDA || cDA /= dDA || aDB /= cDB || bDB /= dDB = Nothing
    | otherwise = foldr1 (liftA2' join) (map Just [mA, mB, mC, mD])
    where
        liftA2' f x y = Monad.join (liftA2 f x y)

isWellFormed :: LabelledMatrix a b c -> Bool
isWellFormed (Matrix m dA dB) = length m == length dA * length dB

assertIsWellFormed :: LabelledMatrix a b c -> Bool
assertIsWellFormed x = assert (isWellFormed x) False

assertAllWellFormed :: (Foldable t) => t (LabelledMatrix a b c) -> Bool
assertAllWellFormed = any (\x -> assert (isWellFormed x) False)

