{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
--{-# LANGUAGE DefaultSignatures  #-}

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
import qualified Data.HashMap.Lazy                                            as HM
import qualified Data.Matrix                                                  as M'
import           Data.Maybe                                                   (fromJust,
                                                                               isJust)
import qualified Data.Set                                                     as S
import           LocalComputation.Utils
import qualified LocalComputation.ValuationAlgebra.QuasiRegular.SemiringValue as Q

-- Typeclasses
import           Control.DeepSeq                                              (NFData)
import           Control.Monad                                                (liftM)
import           Data.Binary                                                  (Binary)
import qualified Data.Binary                                                  as B
import qualified Data.Hashable                                                as H
import           GHC.Generics                                                 (Generic)
import           Numeric.Natural                                              (Natural)

import qualified Data.Massiv.Array                                            as M


data InvalidFormat = DuplicateKeys | NotTotalMapping

{- | A labelled matrix.

A column or row matrix can be created by specifying `LabelledMatrix () b c` or `LabelledMatrix a () c`
-}
data LabelledMatrix a b c = Matrix {
      matrix :: (M.Array M.B M.Ix2 c)
    , dA     :: (HM.HashMap a M.Ix1)
    , dB     :: (HM.HashMap b M.Ix2)
} deriving (NFData, Ord, Generic)

-- TODO: Is Data.HashMap suitable for being serialized? On it's webpage it says it is only suitable for 'in memory' data structures.

-- TODO: Fix orphan instance (just make labelledmatrix an instance of binary directly instead)
instance () => Binary (LabelledMatrix a b c) where
    put = undefined
    get = undefined

instance Read (LabelledMatrix a b c) where
    readsPrec = undefined

instance (Eq a, Eq b, Eq c) => Eq (LabelledMatrix a b c) where
    (==) = undefined

instance (Show a, Show b, Show c) => Show (LabelledMatrix a b c) where
    show = undefined

instance Functor (LabelledMatrix a b) where
    fmap = undefined

-- | Transforms a regular matrix from Data.Matrix into a matrix labelled by Integers.
fromMatrix :: M'.Matrix a -> LabelledMatrix Integer Integer a
fromMatrix m = undefined

-- | Creates a matrix from the given association list.
fromList :: forall a b c . (H.Hashable a, H.Hashable b, Ord a, Ord b)
    => [((a, b), c)]
    -> Either InvalidFormat (LabelledMatrix a b c)
fromList xs = undefined

-- | Creates a matrix from the given association list. Doesn't require the input list to be a total mapping - will
-- fill unset values with the given default element.
fromListDefault :: forall a b c . (H.Hashable a, H.Hashable b, Ord a, Ord b)
    => [((a, b), c)]
    -> c
    -> Either InvalidFormat (LabelledMatrix a b c)
fromListDefault xs default' = undefined

-- | Internal function used to create a list with an optional default element.
fromList' :: forall a b c . (H.Hashable a, H.Hashable b, Ord a, Ord b)
    => [((a, b), c)]
    -> Maybe c
    -> Either InvalidFormat (LabelledMatrix a b c)
fromList' xs defaultElem = undefined

isSymmetric :: (H.Hashable a, Eq c) => LabelledMatrix a a c -> Bool
isSymmetric x | assertIsWellFormed x = undefined
isSymmetric x = undefined

isSquare :: LabelledMatrix a b c -> Bool
isSquare x | assertIsWellFormed x = undefined
isSquare x = undefined

domain :: LabelledMatrix a b c -> (S.Set a, S.Set b)
domain x = undefined

toSquare :: (H.Hashable a, Ord a) => LabelledMatrix a a c -> c -> LabelledMatrix a a c
toSquare x _ | assertIsWellFormed x = undefined
toSquare x zero = undefined

-- | Returns the simplified domain of a square matrix that has the same domain for both labels. Returns Nothing if the given matrix is not square.
squareDomain :: (Eq a) => LabelledMatrix a a c -> Maybe (S.Set a)
squareDomain x | assertIsWellFormed x = undefined
squareDomain x = undefined

-- | Returns the identity matrix created with the given zero and one elements.
identity :: (H.Hashable a, Ord a) => S.Set a -> c -> c -> LabelledMatrix a a c
identity dA zero one = undefined

-- | Extend a matrix to a larger domain, filling spots with the given zero element. Returns Nothing if the domain to extend to is not a superset.
extension :: (H.Hashable a, H.Hashable b, Ord a, Ord b) => LabelledMatrix a b c -> S.Set a -> S.Set b -> c -> Maybe (LabelledMatrix a b c)
extension x _ _ _ | assertIsWellFormed x = undefined
extension m newDA newDB zero = undefined

-- | Project the domain of a matrix down to a new domain. Returns nothing if the given domain is not a subset of the old domain.
project :: (Ord a, Ord b) => LabelledMatrix a b c -> S.Set a -> S.Set b -> Maybe (LabelledMatrix a b c)
project x _ _ | assertIsWellFormed x = undefined
project m newDA newDB = undefined

-- | Returns an element from the matrix. Returns Nothing if the element is not in the domain of the matrix.
find :: (H.Hashable a, H.Hashable b) => (a, b) -> LabelledMatrix a b c -> Maybe c
find _ x | assertIsWellFormed x = undefined
find (a, b) m = undefined

-- | Basic addition on two matrices. Returns Nothing if the provided matrices have different shapes.
add :: (Ord a, Ord b) => (c -> c -> c) -> LabelledMatrix a b c -> LabelledMatrix a b c -> Maybe (LabelledMatrix a b c)
add _ x y | assertAllWellFormed [x, y] = undefined
add addElems m1 m2 = undefined

-- | Basic matrix multiplication on two matrices. Returns Nothing if the provided matrices have the wrong shape for matrix multiplication.
multiply :: (H.Hashable a, H.Hashable b, H.Hashable c) => (d -> d -> d) -> (d -> d -> d) -> d -> LabelledMatrix a b d -> LabelledMatrix b c d -> Maybe (LabelledMatrix a c d)
multiply _ _ _ x y | assertIsWellFormed x || assertIsWellFormed y = undefined
multiply addElems multiplyElems zero m1 m2 = undefined

multiplys :: (H.Hashable a, Functor t, Foldable t) => (c -> c -> c) -> (c -> c -> c) -> c -> t (LabelledMatrix a a c) -> Maybe (LabelledMatrix a a c)
multiplys _ _ _ xs | assertAllWellFormed xs = undefined
multiplys addElems multiplyElems zero xs = undefined

{- | Computes the quasi-inverse of a given matrix, returning Nothing if the given matrix is not square.

This formula is detailed in "Generic Inference" (Pouly and Kohlas, 2012).
-}
quasiInverse :: (H.Hashable a, Ord a, Q.QuasiRegularSemiringValue c, Show a, Show c) => LabelledMatrix a a c -> Maybe (LabelledMatrix a a c)
quasiInverse x | assertIsWellFormed x = undefined
quasiInverse m = undefined

{- Decomposes a square matrix with matching labels on each side into four matrices.

Returns a tuple (A, B, C, D) defined through the following shape:

 ┌─   ─┐
 │ A B │
 │ C D │
 └─   ─┘

Where D is `div n 2` x `div n 2` where `n` is the length of the side of the square matrix. Returns Nothing if the given matrix is empty.
-}
decompose :: (Ord a) => LabelledMatrix a a c -> Maybe (LabelledMatrix a a c, LabelledMatrix a a c, LabelledMatrix a a c, LabelledMatrix a a c)
decompose x | assertIsWellFormed x = undefined
decompose m = undefined


-- | Internal. Joins two disjoint matrices. Input may not be well formed, but the key sets of the maps must be disjoint otherwise an assertion will be thrown. Result may not be well-formed.
join :: (Ord a, Ord b) => LabelledMatrix a b c -> LabelledMatrix a b c -> Maybe (LabelledMatrix a b c)
join m1 m2 = undefined

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
joinSquare m1 m2 m3 m4 = undefined

isWellFormed :: LabelledMatrix a b c -> Bool
isWellFormed m = undefined

assertIsWellFormed :: LabelledMatrix a b c -> Bool
assertIsWellFormed x = assert (isWellFormed x) False

assertAllWellFormed :: (Foldable t) => t (LabelledMatrix a b c) -> Bool
assertAllWellFormed = any (\x -> assert (isWellFormed x) False)
