{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

module LocalComputation.LabelledMatrix
    ( LabelledMatrix
    , fromMatrix
    , fromList
    , fromListDefault
    , empty
    , domain
    , reshape
    , appendRows
    , isSquare
    , toSquare
    , extension
    , project
    , unsafeProject
    , find
    , add
    , unsafeAdd
    , multiply
    , unsafeMultiply
    , multiplys
    , quasiInverse
    , unsafeQuasiInverse
    , decompose
    , joinSquare
    , isWellFormed
    )
where

import           Control.Exception                                            (assert)
import qualified Control.Monad                                                as Monad
import qualified Data.Bimap                                                   as BM
import qualified Data.Map                                                     as Map
import qualified Data.Matrix                                                  as M'
import           Data.Maybe                                                   (fromJust,
                                                                               isJust)
import qualified Data.Set                                                     as S
import           LocalComputation.Utils                                       (assert',
                                                                               fromRight)
import qualified LocalComputation.Utils                                       as Map (fromList'')
import qualified LocalComputation.ValuationAlgebra.QuasiRegular.SemiringValue as Q

-- Typeclasses
import           Control.DeepSeq                                              (NFData)
import           Data.Binary                                                  (Binary,
                                                                               get,
                                                                               put)
import qualified Data.Binary.Get                                              as B
import           GHC.Generics                                                 (Generic)

import           GHC.Records                                                  (HasField,
                                                                               getField)

import qualified Data.List                                                    as L
import           Data.Massiv.Array                                            (Ix2 ((:.)))
import qualified Data.Massiv.Array                                            as M
import qualified Data.Tuple.Extra                                             as T
import           Debug.Trace                                                  (trace)
import qualified LocalComputation.Pretty                                      as P
import qualified LocalComputation.Utils                                       as U

-- TODO: Change asserts to take place *after* the function is called;
-- much safer this way.

data InvalidFormat = DuplicateKeys | NotTotalMapping

{- | A labelled matrix.

A column or row matrix can be created by specifying `LabelledMatrix () b c` or `LabelledMatrix a () c`
-}
data LabelledMatrix a b c = Matrix {
      matrix    :: M.Matrix M.B c
    , rowLabels :: BM.Bimap M.Ix1 a
    , colLabels :: BM.Bimap M.Ix1 b
} deriving (Eq, NFData, Ord, Generic)

instance (Show a, Show b, Show c) => Show (LabelledMatrix a b c) where
    show m = P.showTable $ U.Table headings rows
        where
            matrixRows = M.toLists $ fmap show $ m.matrix

            rowLabels = map (show . snd) $ BM.toAscList m.rowLabels
            colLabels = map (show . snd) $ BM.toAscList m.colLabels

            headings = "" : colLabels
            rows     = zipWith (:) rowLabels matrixRows


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

instance (Binary a, Binary b, Binary c, Ord a, Ord b) => Binary (LabelledMatrix a b c) where
    put m = do
        put (M.toLists m.matrix)
        put (BM.toAscList m.rowLabels)
        put (BM.toAscList m.colLabels)

    get = do
        matrix    <- fmap (M.fromLists' s) get   :: B.Get (M.Matrix M.B c)
        rowLabels <- fmap (BM.fromAscPairList . assert' checkIsAscPairList) get :: B.Get (BM.Bimap M.Ix1 a)
        colLabels <- fmap (BM.fromAscPairList . assert' checkIsAscPairList) get :: B.Get (BM.Bimap M.Ix1 b)
        pure (Matrix matrix rowLabels colLabels)

instance Functor (LabelledMatrix a b) where
    fmap f m = Matrix (M.computeAs M.B $ M.map f m.matrix) m.rowLabels m.colLabels

-- | The computational [s]trategy used for this data structure.
-- This argument is passed to every function that uses the Data.Massiv.Array library. This simply indicates
-- whether or not we wish to parallelize the operation. We choose to use sequential here as the inference
-- process is sufficently parallelized such that it uses all cores anyway.
--
-- If changing to parallel, consider also updating use of 'foldrS' in the code (if it is still being used).
s :: M.Comp
s = M.Seq

-- | Transforms a regular matrix from Data.Matrix into a matrix labelled by Integers.
fromMatrix :: M'.Matrix a -> LabelledMatrix Integer Integer a
fromMatrix m = fromRight $ fromList $ zip indexes (M'.toList m)
    where
        indexes = [(fromIntegral rowLabel, fromIntegral colLabel) | rowLabel <- [0 .. M'.nrows m - 1],
                                                                    colLabel <- [0 .. M'.ncols m - 1]]

-- | \(O(n \log n)\) - Creates a matrix from the given association list.
fromList :: forall a b c . (Ord a, Ord b)
    => [((a, b), c)]
    -> Either InvalidFormat (LabelledMatrix a b c)
fromList xs = fromList' xs Nothing

-- | Creates a matrix from the given association list. Doesn't require the input list to be a total mapping - will
-- fill unset values with the given default element.
fromListDefault :: forall a b c . (Ord a, Ord b)
    => c
    -> [((a, b), c)]
    -> Either InvalidFormat (LabelledMatrix a b c)
fromListDefault defaultElem xs = fromList' xs (Just defaultElem)

-- | Internal function used to create a list with an optional default element.
fromList' :: forall a b c . (Ord a, Ord b)
    => [((a, b), c)]
    -> Maybe c
    -> Either InvalidFormat (LabelledMatrix a b c)
fromList' xs defaultElem
    | Nothing <- m'                                                                           = Left DuplicateKeys
    | Just m <- m', Nothing <- defaultElem, BM.size rowLabels * BM.size colLabels /= length m = Left NotTotalMapping
    | Just m <- m'                                                                            = Right $ Matrix (matrix m) rowLabels colLabels
    where
        rowLabels = enumerate $ S.fromList $ map (fst . fst) xs
        colLabels = enumerate $ S.fromList $ map (snd . fst) xs

        matrix :: Map.Map (a, b) c -> M.Matrix M.B c
        matrix m = M.makeArray s shape (f m)

        shape = M.Sz (BM.size rowLabels :. BM.size colLabels)
        f m (i :. j) = Map.findWithDefault defaultElem' (iLabel, jLabel) m
            where
                iLabel = (BM.!) rowLabels i
                jLabel = (BM.!) colLabels j

        m' :: Maybe (Map.Map (a, b) c)
        m' = Map.fromList'' xs

        defaultElem'
            | Just x <- defaultElem = x
            | otherwise = error "Internal error: should never evaluate."

-- TODO: Should be renamed - we use an overloaded term 'square' here to refer to
-- square matricies, and matrices that have the same labels on rows and columns.
isSquare :: (Eq a, Eq b) => LabelledMatrix a b c -> Bool
isSquare x | assertIsWellFormed x = undefined
isSquare m = m.numRows == m.numCols

domain :: LabelledMatrix a b c -> (S.Set a, S.Set b)
domain m = (m.rowLabelSet, m.colLabelSet)

toSquare :: (Ord a) => LabelledMatrix a a c -> c -> LabelledMatrix a a c
toSquare x _ | assertIsWellFormed x = undefined
toSquare m defaultElem = fromJust $ extension m squareLabelSet squareLabelSet defaultElem
    where
        squareLabelSet = S.union m.rowLabelSet m.colLabelSet

-- | Reshapes a matrix, filling empty spots with the given default element.
reshape :: forall a b c . (Ord a, Ord b) => c -> LabelledMatrix a b c -> S.Set a -> S.Set b -> LabelledMatrix a b c
reshape defaultElem m rowLabelSet colLabelSet = Matrix matrix rowLabels colLabels
    where
        rowLabels = enumerate rowLabelSet
        colLabels = enumerate colLabelSet
        nRows = BM.size rowLabels
        nCols = BM.size colLabels

        -- TODO: Clean up code
        matrix :: M.Matrix M.B c
        matrix
            | nRows == 0 || nCols == 0 = emptyMatrix
            | otherwise = M.fromLists' s [getRow i (neededJs [0 .. nCols - 1]) | i <- [0 .. nRows - 1]]
            where
                emptyMatrix :: M.Matrix M.B c
                emptyMatrix = M.makeArray s (M.Sz2 nRows nCols) $ \_ -> defaultElem

                getRow i cache
                    | Just oldI <- oldI' = [getEntry oldI (cache !! j) | j <- [0 .. nCols - 1]]
                    | otherwise          = [defaultElem                | _ <- [0 .. nCols - 1]]
                    where
                        iLabel = (BM.!) rowLabels i
                        oldI' = BM.lookupR iLabel m.rowLabels

                getEntry oldI oldJ'
                    | Just oldJ <- oldJ' = (M.!) m.matrix (oldI :. oldJ)
                    | otherwise          = defaultElem

                neededJs idxs = map oldJ' idxs
                    where
                        jLabel j = (BM.!) colLabels j
                        oldJ' j = BM.lookupR (jLabel j) m.colLabels

-- | Extend a matrix to a larger domain, filling spots with the given default element. Returns Nothing if the domain to extend to is not a superset.
extension :: (Ord a, Ord b) => LabelledMatrix a b c -> S.Set a -> S.Set b -> c -> Maybe (LabelledMatrix a b c)
extension x _ _ _ | assertIsWellFormed x = undefined
extension m rowLabelSet colLabelSet defaultElem
    | m.rowLabelSet == rowLabelSet && m.colLabelSet == colLabelSet = Just $ m
    | m.rowLabelSet `S.isSubsetOf` rowLabelSet && m.colLabelSet `S.isSubsetOf` colLabelSet = Just $ reshape defaultElem m rowLabelSet colLabelSet
    | otherwise = Nothing

-- | Project the domain of a matrix down to a new domain. Returns nothing if the given domain is not a subset of the old domain.
project :: (Ord a, Ord b) => LabelledMatrix a b c -> S.Set a -> S.Set b -> Maybe (LabelledMatrix a b c)
project m _ _ | assertIsWellFormed m = undefined
project m rowLabelSet colLabelSet
    | not (S.isSubsetOf rowLabelSet m.rowLabelSet) || not (S.isSubsetOf colLabelSet m.colLabelSet) = Nothing
    | otherwise = Just $ reshape unusedArg m rowLabelSet colLabelSet
    where
        unusedArg = error "Should never be evaluated"

-- | Returns an element from the matrix. Returns Nothing if the element is not in the domain of the matrix.
find :: (Ord a, Ord b) => (a, b) -> LabelledMatrix a b c -> Maybe c
find _ x | assertIsWellFormed x = undefined
find (a, b) m = do
    aIndex <- BM.lookupR a m.rowLabels
    bIndex <- BM.lookupR b m.colLabels

    pure $ M.index' m.matrix (aIndex :. bIndex)

-- | Basic addition on two matrices. Returns Nothing if the provided matrices have different shapes.
add :: (Ord a, Ord b)
    => (c -> c -> c)
    -> LabelledMatrix a b c
    -> LabelledMatrix a b c
    -> Maybe (LabelledMatrix a b c)
add _ x y | assertAllWellFormed [x, y] = undefined
add addElems m1 m2 = pointwise addElems m1 m2

-- | Perform an operation over the matrix pointwise.
pointwise :: (Eq a, Eq b)
    => (c -> d -> e)
    -> LabelledMatrix a b c
    -> LabelledMatrix a b d
    -> Maybe (LabelledMatrix a b e)
pointwise _ m1 m2 | assertIsWellFormed m1 || assertIsWellFormed m2 = undefined
pointwise f m1 m2
    | m1.rowLabels /= m2.rowLabels || m1.colLabels /= m2.colLabels = Nothing
    | otherwise = Just $ Matrix matrix m1.rowLabels m1.colLabels
    where
        matrix = M.computeAs M.B $ M.zipWith f m1.matrix m2.matrix

-- | Basic matrix multiplication on two matrices. Returns Nothing if the provided matrices have the wrong shape for matrix multiplication.
multiply :: forall a b c d . (Eq a, Eq b, Eq c)
    => d
    -> (d -> d -> d)
    -> (d -> d -> d)
    -> LabelledMatrix a b d
    -> LabelledMatrix b c d
    -> Maybe (LabelledMatrix a c d)
multiply _ _ _ m1 m2 | assertIsWellFormed m1 || assertIsWellFormed m2 = undefined
multiply zero addElems multiplyElems m1 m2
    | m1.colLabels /= m2.rowLabels = Nothing
    | m1.numRows == 0 || m1.numCols == 0 || m2.numRows == 0 || m2.numCols == 0 = Just $ Matrix emptyMatrix m1.rowLabels m2.colLabels
    | otherwise = Just $ Matrix matrix m1.rowLabels m2.colLabels

    where
        emptyMatrix :: M.Matrix M.B d
        emptyMatrix = M.makeArray s (M.Sz2 m1.numRows m2.numCols) $ \_ -> zero

        matrix :: M.Matrix M.B d
        matrix = M.makeArray s (M.Sz2 m1.numRows m2.numCols) f

        f (i :. j) = M.foldrS addElems zero $ M.zipWith multiplyElems row col
            where
                row = (M.!>) m1.matrix i
                col = (M.<!) m2.matrix j

multiplys :: (Eq a, Functor t, Foldable t)
    => c
    -> (c -> c -> c)
    -> (c -> c -> c)
    -> t (LabelledMatrix a a c)
    -> Maybe (LabelledMatrix a a c)
multiplys _ _ _ ms | assertAllWellFormed ms = undefined
multiplys zero addElems multiplyElems ms
    | null ms = Nothing
    | otherwise = foldl1 (liftA2' (multiply zero addElems multiplyElems)) (fmap Just ms)
    where
        liftA2' f x y = Monad.join (liftA2 f x y)

{- | Computes the quasi-inverse of a given matrix, returning Nothing if the given matrix is not square.

This formula is detailed in "Generic Inference" (Pouly and Kohlas, 2012).
-}
quasiInverse :: (Ord a, Q.QSemiringValue c, Show a, Show c)
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
        f = addQ e (multiplys' [d, bStar, c])
        bStar = unsafeQuasiInverse b
        fStar = unsafeQuasiInverse f

        newB = addQ bStar (multiplys' [bStar, c, fStar, d, bStar])
        newC = multiplys' [bStar, c, fStar]
        newD = multiplys' [fStar, d, bStar]
        newE = fStar

        addQ x y = unsafeAdd Q.add x y
        multiplys' xs = fromJust $ multiplys Q.zero Q.add Q.multiply xs

-- TODO: This function could probably be made a lot faster by not calling project.
-- for example we could have a new function that just operates on indices and splits on them.

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
    | otherwise = Just (project' m aRows    aCols, project' m aRows    notACols,
                        project' m notARows aCols, project' m notARows notACols)

    where
        aNumRows = m.numRows `div` 2 + m.numRows `mod` 2
        aNumCols = m.numCols `div` 2 + m.numCols `mod` 2

        (aRows, notARows) = T.both (Map.keysSet . BM.toMapR) $ BM.partition (\i _ -> i < aNumRows) m.rowLabels
        (aCols, notACols) = T.both (Map.keysSet . BM.toMapR) $ BM.partition (\i _ -> i < aNumCols) m.colLabels

        project' x y z = fromJust $ project x y z

-- TODO: Reduce code duplication by calling `appendRows` and creating a new `appendCols` function.
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
    -- Row / column labels match where they coincide in the overall square matrix.
    |    a.rowLabels /= b.rowLabels
      || c.rowLabels /= d.rowLabels
      || a.colLabels /= c.colLabels
      || b.colLabels /= d.colLabels = trace "Issue likely caused by new assumption" Nothing
    -- All labels at a higher index are larger
    -- (this also handles checking disjointness)
    |    (fst $ BM.findMaxR a.rowLabels) >= (fst $ BM.findMinR c.rowLabels)
      || (fst $ BM.findMaxR a.colLabels) >= (fst $ BM.findMinR b.colLabels) = Nothing
    | otherwise = Just $ Matrix matrix rowLabels colLabels
    where
        matrix = append 2 (append 1 a.matrix b.matrix)
                          (append 1 c.matrix d.matrix)

        rowLabels = BM.fromAscPairList $ (++) (BM.toAscList a.rowLabels)
                                              (BM.toAscList $ BM.mapMonotonic (+ a.numRows) c.rowLabels)

        colLabels = BM.fromAscPairList $ (++) (BM.toAscList a.colLabels)
                                              (BM.toAscList $ BM.mapMonotonic (+ a.numCols) b.colLabels)

        append = ((M.computeAs M.B .) .) . M.append'

-- TODO: Because of (unnecesary?) assumption about both the indexes and labels being sorted
-- such that both ascend simultaenously, this function is a lot more complicated than it needs to be.
appendRows :: (Ord a, Eq b) => LabelledMatrix a b c -> LabelledMatrix a b c -> Maybe (LabelledMatrix a b c)
appendRows m1 m2 | assertAllWellFormed  [m1, m2] = undefined
appendRows m1 m2
    | m1.colLabels /= m2.colLabels = Nothing
    | not $ S.disjoint m1.rowLabelSet m2.rowLabelSet = Nothing
    | otherwise = Just $ Matrix matrix rowLabels m1.colLabels
    where
        rows = L.sortOn fst $ zip (map snd $ BM.toAscList m1.rowLabels ++ BM.toAscList m2.rowLabels)
                                  (M.toLists $ append 2 m1.matrix m2.matrix)

        matrix = M.fromLists' s $ map snd rows

        rowLabels = enumerate $ S.union (m1.rowLabelSet) (m2.rowLabelSet)

        append = ((M.computeAs M.B .) .) . M.append'

empty :: LabelledMatrix a b c
empty = Matrix M.empty BM.empty BM.empty

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------
enumerate :: (Ord a) => S.Set a -> BM.Bimap M.Ix1 a
enumerate xs = (BM.fromAscPairList . assert' checkIsAscPairList) $ zip [M.Ix1 0..] (S.toAscList xs)

--------------------------------------------------------------------------------
-- Invariants
--------------------------------------------------------------------------------
{- | Returns true if the matrix satisfies a set of invariants that we wish
to maintain between operations.

Entires in bimap strictly increasing in both arguments is unnecessary and should
likely be removed.
-}
isWellFormed :: (Eq a, Eq b) => LabelledMatrix a b c -> Bool
isWellFormed m
    -- Shape of matrix matches number of row and column labels.
    | m.numRows /= BM.size m.rowLabels || m.numCols /= BM.size m.colLabels = False
    -- No labels are mapped to indexes outside the bounds of the matrix
    | any (\v -> v < 0 || v >= m.numRows) (BM.keys m.rowLabels)            = False
    | any (\v -> v < 0 || v >= m.numCols) (BM.keys m.colLabels)            = False
    -- Entries in the bimap are strictly increasing in both arguments
    | BM.toAscList m.rowLabels /= map T.swap (BM.toAscListR m.rowLabels)   = False
    | BM.toAscList m.colLabels /= map T.swap (BM.toAscListR m.colLabels)   = False
    | otherwise = True

assertIsWellFormed :: (Eq a, Eq b) => LabelledMatrix a b c -> Bool
assertIsWellFormed x = assert (isWellFormed x) False

assertAllWellFormed :: (Eq a, Eq b, Foldable t) => t (LabelledMatrix a b c) -> Bool
assertAllWellFormed = any (\x -> assert (isWellFormed x) False)

checkIsAscPairList :: (Ord a, Ord b) => [(a, b)] -> Bool
checkIsAscPairList []            = True
checkIsAscPairList ((x, y) : zs) = all (\(x', y') -> x < x' && y < y') zs

--------------------------------------------------------------------------------
-- Unsafe variants
--------------------------------------------------------------------------------
unsafeProject :: (Ord a, Ord b) => LabelledMatrix a b c -> S.Set a -> S.Set b -> LabelledMatrix a b c
unsafeProject = ((fromJust .) .) . project

unsafeQuasiInverse :: (Ord a, Q.QSemiringValue c, Show a, Show c)
    => LabelledMatrix a a c
    -> LabelledMatrix a a c
unsafeQuasiInverse = fromJust . quasiInverse

-- | Basic addition on two matrices. Returns Nothing if the provided matrices have different shapes.
unsafeAdd :: (Ord a, Ord b)
    => (c -> c -> c)
    -> LabelledMatrix a b c
    -> LabelledMatrix a b c
    -> LabelledMatrix a b c
unsafeAdd = ((fromJust .) .) . add

unsafeMultiply :: forall a b c d . (Eq a, Eq b, Eq c)
    => d
    -> (d -> d -> d)
    -> (d -> d -> d)
    -> LabelledMatrix a b d
    -> LabelledMatrix b c d
    -> LabelledMatrix a c d
unsafeMultiply = ((((fromJust .) .) .) .) . multiply
