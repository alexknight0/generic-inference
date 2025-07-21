{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

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
import qualified Data.Map                                                     as Map
import qualified Data.Set                                                     as S
-- import qualified Data.HashMap.Lazy                                            as HM
-- import qualified Data.HashSet                                                 as HS
import qualified Data.Bimap                                                   as BM
import qualified Data.List                                                    as L
import qualified Data.Matrix                                                  as M'
import           Data.Maybe                                                   (fromJust,
                                                                               isJust)
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

import           GHC.Records                                                  (HasField,
                                                                               getField)

import           Data.Massiv.Array                                            (Ix2 ((:.)))
import qualified Data.Massiv.Array                                            as M
import           Debug.Trace                                                  (trace)


data InvalidFormat = DuplicateKeys | NotTotalMapping

{- | A labelled matrix.

A column or row matrix can be created by specifying `LabelledMatrix () b c` or `LabelledMatrix a () c`
-}
data LabelledMatrix a b c = Matrix {
      matrix    :: M.Matrix M.B c
    , rowLabels :: BM.Bimap M.Ix1 a
    , colLabels :: BM.Bimap M.Ix1 b
} deriving (NFData, Ord, Generic)

-- | O(1) accessor for number of rows
instance HasField "numRows" (LabelledMatrix a b c) M.Ix1 where
    getField m = numRows
        where M.Sz (numRows M.:. _) = M.size m.matrix

-- | O(1) accessor for number of columns
instance HasField "numCols" (LabelledMatrix a b c) M.Ix1 where
    getField m = numCols
        where M.Sz (_ M.:. numCols) = M.size m.matrix

-- | O(n) accessor for row label set where n is the number of row labels.
instance HasField "rowLabelSet" (LabelledMatrix a b c) (S.Set a) where
    getField m = Map.keysSet $ BM.toMapR m.rowLabels

-- | O(n) accessor for column label set where n is the number of column labels.
instance HasField "colLabelSet" (LabelledMatrix a b c) (S.Set b) where
    getField m = Map.keysSet $ BM.toMapR m.colLabels

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

-- | The computational [s]trategy used for this data structure.
-- This argument is passed to every function that uses the Data.Massiv.Array library. This simply indicates
-- whether or not we wish to parallelize the operation. We choose to use sequential here as the inference
-- process is sufficently parallelized such that it uses all cores anyway.
s :: M.Comp
s = M.Seq

-- | Transforms a regular matrix from Data.Matrix into a matrix labelled by Integers.
fromMatrix :: M'.Matrix a -> LabelledMatrix Integer Integer a
fromMatrix m = undefined

-- | Creates a matrix from the given association list.
-- Unsafe - throws an error if a duplicate element is found or the list doesn't provide a total mapping.
fromList :: forall a b c . (Ord a, Ord b, H.Hashable a, H.Hashable b)
    => [((a, b), c)]
    -> Either InvalidFormat (LabelledMatrix a b c)
fromList xs = Right $ Matrix matrix rowLabels colLabels

    where
        rowLabels = enumerate $ S.fromList $ map (fst . fst) xs
        colLabels = enumerate $ S.fromList $ map (snd . fst) xs

        xsWithIndexes = map (\((a, b), c) -> ((rowLabels BM.!> a, colLabels BM.!> b), c)) xs

        xsWithIndexesFlattened = map snd $ L.sortOn fst xsWithIndexes

        matrix :: M.Matrix M.B c
        matrix = M.resize' size (M.fromList s xsWithIndexesFlattened)
            where
                size :: M.Sz M.Ix2
                size = (M.Sz $ (BM.size rowLabels) :. (BM.size colLabels))

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

project' :: (Ord a, Ord b, H.Hashable a, H.Hashable b) => LabelledMatrix a b c -> S.Set a -> S.Set b -> Maybe (LabelledMatrix a b c)
project' m _ _ | assertIsWellFormed m = undefined
project' m rowLabelSet colLabelSet
    | not (S.isSubsetOf rowLabelSet m.rowLabelSet) || not (S.isSubsetOf colLabelSet m.colLabelSet) = Nothing
    | otherwise = Just $ Matrix matrix rowLabels colLabels
    where
        rowLabels = enumerate rowLabelSet
        colLabels = enumerate colLabelSet

        shape = M.Sz (length rowLabelSet :. length colLabelSet)
        matrix = M.makeArray s shape f
        f (i :. j) = (M.!) m.matrix (oldI :. oldJ)
            where
                iLabel = (BM.!) rowLabels i
                jLabel = (BM.!) colLabels j

                oldI = (BM.!>) m.rowLabels iLabel
                oldJ = (BM.!>) m.colLabels jLabel

-- | Returns an element from the matrix. Returns Nothing if the element is not in the domain of the matrix.
find :: (H.Hashable a, H.Hashable b) => (a, b) -> LabelledMatrix a b c -> Maybe c
find _ x | assertIsWellFormed x = undefined
find (a, b) m = undefined

-- | Basic addition on two matrices. Returns Nothing if the provided matrices have different shapes.
add :: (Ord a, Ord b) => (c -> c -> c) -> LabelledMatrix a b c -> LabelledMatrix a b c -> Maybe (LabelledMatrix a b c)
add _ x y | assertAllWellFormed [x, y] = undefined
add addElems m1 m2 = undefined

-- | Basic addition on two matrices. Returns Nothing if the provided matrices have different shapes.
add' :: (Ord a, Ord b) => LabelledMatrix a b c -> LabelledMatrix a b c -> Maybe (LabelledMatrix a b c)
add' x y | assertAllWellFormed [x, y] = undefined
add' m1 m2 = undefined

-- | Basic matrix multiplication on two matrices. Returns Nothing if the provided matrices have the wrong shape for matrix multiplication.
multiply :: (H.Hashable a, H.Hashable b, H.Hashable c) => (d -> d -> d) -> (d -> d -> d) -> d -> LabelledMatrix a b d -> LabelledMatrix b c d -> Maybe (LabelledMatrix a c d)
multiply _ _ _ x y | assertIsWellFormed x || assertIsWellFormed y = undefined
multiply addElems multiplyElems zero m1 m2 = undefined

-- | Basic matrix multiplication on two matrices. Returns Nothing if the provided matrices have the wrong shape for matrix multiplication.
multiply' :: forall a b c d . (H.Hashable a, H.Hashable b, H.Hashable c, Num d)
    => d
    -> LabelledMatrix a b d
    -> LabelledMatrix b c d
    -> Maybe (LabelledMatrix a c d)
multiply' _ m1 m2 | assertIsWellFormed m1 || assertIsWellFormed m2 = undefined
multiply' zero m1 m2
    | m1.colLabelSet /= m2.rowLabelSet = Nothing
    | m1.numRows == 0 || m1.numCols == 0 || m2.numRows == 0 || m2.numCols == 0 = Just $ Matrix emptyMatrix m1.rowLabels m2.colLabels
    | otherwise = Just $ Matrix (m1.matrix M.!><! m2.matrix) m1.rowLabels m2.colLabels

    where
        emptyMatrix :: M.Matrix M.B d
        emptyMatrix = M.makeArray s (M.Sz2 m1.numRows m2.numCols) $ \_ -> zero

{- | Empty matrix behaviour


>>> emptyMat = M.makeArray M.Seq (M.Sz2 5 0) $ \(i M.:. j) -> error "unreachable"
>>> otherMat = M.makeArray M.Seq (M.Sz2 0 4) $ \(i M.:. j) -> i + j
>>> M.multiplyMatrices emptyMat otherMat

-}

{-

>>> exampleMat :: M.Array M.U M.Ix2 Int; exampleMat = M.makeArray M.Seq (M.Sz2 5 4) $ \(i M.:. j) -> i + j
>>> M.Sz (x M.:. y) = M.size (exampleMat)
>>> :t x
x :: Int

-}

multiplys :: (H.Hashable a, Functor t, Foldable t) => (c -> c -> c) -> (c -> c -> c) -> c -> t (LabelledMatrix a a c) -> Maybe (LabelledMatrix a a c)
multiplys _ _ _ xs | assertAllWellFormed xs = undefined
multiplys addElems multiplyElems zero xs = undefined

multiplys' :: (H.Hashable a, Functor t, Foldable t, Num c)
    => c -> t (LabelledMatrix a a c) -> Maybe (LabelledMatrix a a c)
multiplys' _ ms | assertAllWellFormed ms = undefined
multiplys' zero ms
    | null ms = Nothing
    | otherwise = foldl1 (liftA2' (multiply' zero)) (fmap Just ms)
    where
        liftA2' f x y = Monad.join (liftA2 f x y)

{- | Computes the quasi-inverse of a given matrix, returning Nothing if the given matrix is not square.

This formula is detailed in "Generic Inference" (Pouly and Kohlas, 2012).
-}
quasiInverse :: (H.Hashable a, Ord a, Q.QuasiRegularSemiringValue c, Show a, Show c)
    => LabelledMatrix a a c
    -> Maybe (LabelledMatrix a a c)
quasiInverse x | assertIsWellFormed x = undefined
quasiInverse m
    | m.numRows /= m.numCols = Nothing
    | m.numRows == 0 = Just $ m
    | m.numRows == 1 = Just $ fmap Q.quasiInverse m
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

-- TODO: I'm not sure if it matters if the decomposition for quasiinverse has the 'A' component contain a set of row labels
-- and column labels that are the same as it would in the case of a graphical split like below. If we get errors, maybe
-- consider looking closer at this.

-- TODO: This function could probably be made a lot faster by not calling project.

{- Decomposes a square matrix with matching labels on each side into four matrices.

Returns a tuple (A, B, C, D) defined through the following shape:

 ┌─   ─┐
 │ A B │
 │ C D │
 └─   ─┘

Where D is of shape `div numRows 2` x `div numCols 2`.

Returns Nothing if the given matrix is empty.   TODO: it probably doesn't have to return Nothing here.
-}
decompose :: (Ord a) => LabelledMatrix a a c -> Maybe (LabelledMatrix a a c, LabelledMatrix a a c, LabelledMatrix a a c, LabelledMatrix a a c)
decompose x | assertIsWellFormed x = undefined
decompose m
    | m.numRows == 0 && m.numCols == 0 = Nothing
    | otherwise = do
        aRows <- fmap S.fromList $ takeN aNumRows (S.toList $ m.rowLabelSet)
        aCols <- fmap S.fromList $ takeN aNumCols (S.toList $ m.colLabelSet)

        let notARows = m.rowLabelSet `S.difference` aRows
            notACols = m.colLabelSet `S.difference` aCols

        pure (project' m aRows    aCols, project' m aRows    notACols,
              project' m notARows aCols, project' m notARows notACols)

        where
            project' x y z = fromJust $ project x y z

            takeN :: Natural -> [d] -> Maybe [d]
            takeN 0 _      = Just []
            takeN _ []     = Nothing
            takeN n (x:xs) = Just (x:) <*> takeN (n-1) xs

            aNumRows = fromIntegral $ m.numRows `div` 2 + m.numRows `mod` 2
            aNumCols = fromIntegral $ m.numCols `div` 2 + m.numCols `mod` 2


-- | Internal. Joins two disjoint matrices. Input may not be well formed, but the key sets of the maps must be disjoint otherwise an assertion will be thrown. Result may not be well-formed.
join :: (Ord a, Ord b) => LabelledMatrix a b c -> LabelledMatrix a b c -> Maybe (LabelledMatrix a b c)
join m1 m2 = undefined

-- TODO: THIS ASSUMES THE BIMAPS ARE INDENTICAL. This is quite different from just assuming
-- the domains are the same as the function did before - but this new assumption could lead
-- to performance improvements if it is true all the time (which i think it might be!)
-- If we encounter issues, check that this is not the cause of error.

{- Joins four matrices, returning Nothing if the matrices are not arranged in a square.

For inputs A -> B -> C -> D returns:

 ┌─   ─┐
 │ A B │
 │ C D │
 └─   ─┘

Note that this does not indicate that the resulting matrix is a square matrix.
-}
joinSquare :: forall a b c . (Ord a, Ord b) => LabelledMatrix a b c -> LabelledMatrix a b c -> LabelledMatrix a b c -> LabelledMatrix a b c -> Maybe (LabelledMatrix a b c)
joinSquare a b c d | assertAllWellFormed [a, b, c, d] = undefined
joinSquare a b c d
    -- Labels are disjoint where they should be.
    |    not (S.disjoint a.rowLabelSet c.rowLabelSet)
      || not (S.disjoint a.colLabelSet d.colLabelSet) = Nothing
    -- Row / column labels exactly match where they coincide in the overall square matrix.
    |    a.rowLabels /= b.rowLabels
      || c.rowLabels /= d.rowLabels
      || a.colLabels /= c.colLabels
      || b.colLabels /= d.colLabels = trace "Issue likely caused by new assumption" Nothing
    | otherwise = Just $ Matrix matrix rowLabels colLabels
    where
        matrix = append 2 (append 1 a.matrix b.matrix)
                          (append 1 c.matrix d.matrix)

        -- TODO:
        -- We can't use BM.fromAscPairList here unless we constrain the input of the
        -- function `joinSquare` to only allow, for example, a `c` that has a `c.rowLabelSet`
        -- such that `all (\x -> x > allOf a.rowLabelSet) c.rowLabelSet`. Otherwise, even
        -- though the index is monotonically increasing, the label value might not be.
        -----
        -- Regardless of the above discussion, we have an extra property of 'malformed' to
        -- consider here. We can guarantee that the matrix we create has the property that
        -- the ordering of both keys in each bimap is identical. While this seems strict,
        -- I worry that we might actually be causing errors by not enforcing this restriction
        -- currently. It is easy to lose track of which elements are being multiplied,
        -- and if the elements our labels correspond to are actually staying the same.
        rowLabels = BM.fromAscPairList $ (++) (BM.toAscList a.rowLabels)
                                              (BM.toAscList $ BM.mapMonotonic (+ a.numRows) c.rowLabels)

        colLabels = BM.fromAscPairList $ (++) (BM.toAscList a.colLabels)
                                              (BM.toAscList $ BM.mapMonotonic (+ a.numCols) c.colLabels)

        append = ((M.computeAs M.B .) .) . M.append'

enumerate :: (Ord a) => S.Set a -> BM.Bimap M.Ix1 a
enumerate xs = BM.fromAscPairList $ zip [M.Ix1 0..] (S.toAscList xs)

isWellFormed :: LabelledMatrix a b c -> Bool
isWellFormed m
    -- Shape of matrix matches number of row and column labels.
    | m.numRows /= BM.size m.rowLabels || m.numCols /= BM.size m.colLabels = False
    -- No labels are mapped to indexes outside the bounds of the matrix
    | any (\v -> v < 0 || v >= m.numRows) (BM.keys m.rowLabels) = False
    | any (\v -> v < 0 || v >= m.numCols) (BM.keys m.colLabels) = False
    | otherwise = True

assertIsWellFormed :: LabelledMatrix a b c -> Bool
assertIsWellFormed x = assert (isWellFormed x) False

assertAllWellFormed :: (Foldable t) => t (LabelledMatrix a b c) -> Bool
assertAllWellFormed = any (\x -> assert (isWellFormed x) False)

