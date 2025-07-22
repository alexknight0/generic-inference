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
import           LocalComputation.Utils                                       (assert',
                                                                               fromRight,
                                                                               nubWithBy)
import qualified LocalComputation.Utils                                       as Map (fromList'')
import qualified LocalComputation.ValuationAlgebra.QuasiRegular.SemiringValue as Q

-- Typeclasses
import           Control.DeepSeq                                              (NFData)
import           Control.Monad                                                (liftM)
import           Data.Binary                                                  (Binary,
                                                                               get,
                                                                               put)
import qualified Data.Binary.Builder                                          as B
import qualified Data.Binary.Get                                              as B
import qualified Data.Binary.Put                                              as B
import qualified Data.ByteString.Builder                                      as BS
import qualified Data.Hashable                                                as H
import           GHC.Generics                                                 (Generic)
import           Numeric.Natural                                              (Natural)

import           GHC.Records                                                  (HasField,
                                                                               getField)

import           Data.Massiv.Array                                            (Ix2 ((:.)))
import qualified Data.Massiv.Array                                            as M
import qualified Data.Tuple.Extra                                             as T
import           Debug.Trace                                                  (trace)


data InvalidFormat = DuplicateKeys | NotTotalMapping

{- | A labelled matrix.

A column or row matrix can be created by specifying `LabelledMatrix () b c` or `LabelledMatrix a () c`
-}
data LabelledMatrix a b c = Matrix {
      matrix    :: M.Matrix M.B c
    , rowLabels :: BM.Bimap M.Ix1 a
    , colLabels :: BM.Bimap M.Ix1 b
} deriving (Show, Eq, NFData, Ord, Generic)

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

-- newtype BimapWrapper a b = BimapWrapper (BM.Bimap a b) deriving (Generic, Binary)

-- deriving instance (Binary a, Binary b) => Binary (BM.Bimap a b)


-- TODO: Fix orphan instance (just make labelledmatrix an instance of binary directly instead)
instance (Binary a, Binary b, Binary c, Ord a, Ord b) => Binary (LabelledMatrix a b c) where
    put m = do
        put (M.toLists m.matrix)
        put (BM.toAscList m.rowLabels)
        put (BM.toAscList m.colLabels)

    get = do
        matrix    <- fmap (M.fromLists' s) get   :: B.Get (M.Matrix M.B c)
        rowLabels <- fmap BM.fromAscPairList get :: B.Get (BM.Bimap M.Ix1 a)
        colLabels <- fmap BM.fromAscPairList get :: B.Get (BM.Bimap M.Ix1 b)
        pure (Matrix matrix rowLabels colLabels)

-- matrix = B.toLazyByteString $ M.toBuilder (B.fromLazyByteString . B.runPut . put) m.matrix

instance Functor (LabelledMatrix a b) where
    fmap f m = Matrix (M.computeAs M.B $ M.map f m.matrix) m.rowLabels m.colLabels

-- | The computational [s]trategy used for this data structure.
-- This argument is passed to every function that uses the Data.Massiv.Array library. This simply indicates
-- whether or not we wish to parallelize the operation. We choose to use sequential here as the inference
-- process is sufficently parallelized such that it uses all cores anyway.
s :: M.Comp
s = M.Seq

-- | Transforms a regular matrix from Data.Matrix into a matrix labelled by Integers.
fromMatrix :: M'.Matrix a -> LabelledMatrix Integer Integer a
fromMatrix m = fromRight $ fromList $ zip indexes (M'.toList m)
    where
        indexes = [(fromIntegral rowLabel, fromIntegral colLabel) | rowLabel <- [0 .. M'.nrows m],
                                                                    colLabel <- [0 .. M'.ncols m]]

-- | \(O(n \log n)\) - Creates a matrix from the given association list.
fromList :: forall a b c . (Ord a, Ord b, H.Hashable a, H.Hashable b)
    => [((a, b), c)]
    -> Either InvalidFormat (LabelledMatrix a b c)
fromList xs = fromList' xs Nothing

-- | Creates a matrix from the given association list. Doesn't require the input list to be a total mapping - will
-- fill unset values with the given default element.
fromListDefault :: forall a b c . (H.Hashable a, H.Hashable b, Ord a, Ord b)
    => [((a, b), c)]
    -> c
    -> Either InvalidFormat (LabelledMatrix a b c)
fromListDefault xs defaultElem = fromList' xs (Just defaultElem)

-- | Internal function used to create a list with an optional default element.
fromList' :: forall a b c . (H.Hashable a, H.Hashable b, Ord a, Ord b)
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

isSymmetric :: (H.Hashable a, Eq c) => LabelledMatrix a a c -> Bool
isSymmetric x | assertIsWellFormed x = undefined
isSymmetric x = undefined

-- TODO: Should be renamed - we use an overloaded term 'square' here to refer to
-- square matricies, and matrices that have the same labels on rows and columns.
isSquare :: (Eq a, Eq b) => LabelledMatrix a b c -> Bool
isSquare x | assertIsWellFormed x = undefined
isSquare m = m.numRows == m.numCols

domain :: LabelledMatrix a b c -> (S.Set a, S.Set b)
domain m = (m.rowLabelSet, m.colLabelSet)

toSquare :: (H.Hashable a, Ord a) => LabelledMatrix a a c -> c -> LabelledMatrix a a c
toSquare x _ | assertIsWellFormed x = undefined
toSquare m defaultElem = fromJust $ extension m squareLabelSet squareLabelSet defaultElem
    where
        squareLabelSet = S.union m.rowLabelSet m.colLabelSet

-- | Returns the simplified domain of a square matrix that has the same domain for both labels. Returns Nothing if the given matrix is not square.
squareDomain :: (Eq a) => LabelledMatrix a a c -> Maybe (S.Set a)
squareDomain x | assertIsWellFormed x = undefined
squareDomain x = undefined

-- | Returns the identity matrix created with the given zero and one elements.
identity :: (H.Hashable a, Ord a) => S.Set a -> c -> c -> LabelledMatrix a a c
identity dA zero one = undefined

-- | Reshapes a matrix, filling empty spots with the given default element.
reshape :: (Ord a, Ord b) => c -> LabelledMatrix a b c -> S.Set a -> S.Set b -> LabelledMatrix a b c
reshape defaultElem m rowLabelSet colLabelSet = Matrix matrix rowLabels colLabels
    where
        rowLabels = enumerate rowLabelSet
        colLabels = enumerate colLabelSet

        shape = M.Sz (length rowLabelSet :. length colLabelSet)
        matrix = M.makeArray s shape f
        f (i :. j)
            | Just oldI <- oldI', Just oldJ <- oldJ' = (M.!) m.matrix (oldI :. oldJ)
            | otherwise = defaultElem
            where
                iLabel = (BM.!) rowLabels i
                jLabel = (BM.!) colLabels j

                oldI' = BM.lookupR iLabel m.rowLabels
                oldJ' = BM.lookupR jLabel m.colLabels

-- | Extend a matrix to a larger domain, filling spots with the given default element. Returns Nothing if the domain to extend to is not a superset.
extension :: (H.Hashable a, H.Hashable b, Ord a, Ord b) => LabelledMatrix a b c -> S.Set a -> S.Set b -> c -> Maybe (LabelledMatrix a b c)
extension x _ _ _ | assertIsWellFormed x = undefined
extension m rowLabelSet colLabelSet defaultElem
    | m.rowLabelSet `S.isSubsetOf` rowLabelSet && m.colLabelSet `S.isSubsetOf` colLabelSet = Just $ reshape defaultElem m rowLabelSet colLabelSet
    | otherwise = Nothing

-- | Project the domain of a matrix down to a new domain. Returns nothing if the given domain is not a subset of the old domain.
project :: (Ord a, Ord b) => LabelledMatrix a b c -> S.Set a -> S.Set b -> Maybe (LabelledMatrix a b c)
project x _ _ | assertIsWellFormed x = undefined
project m newDA newDB = undefined

project' :: (Ord a, Ord b, H.Hashable a, H.Hashable b) => LabelledMatrix a b c -> S.Set a -> S.Set b -> Maybe (LabelledMatrix a b c)
project' m _ _ | assertIsWellFormed m = undefined
project' m rowLabelSet colLabelSet
    | not (S.isSubsetOf rowLabelSet m.rowLabelSet) || not (S.isSubsetOf colLabelSet m.colLabelSet) = Nothing
    | otherwise = Just $ reshape unusedArg m rowLabelSet colLabelSet
    where
        unusedArg = error "Should never be evaluated"

-- | Returns an element from the matrix. Returns Nothing if the element is not in the domain of the matrix.
find :: (Ord a, Ord b, H.Hashable a, H.Hashable b) => (a, b) -> LabelledMatrix a b c -> Maybe c
find _ x | assertIsWellFormed x = undefined
find (a, b) m = do
    aIndex <- BM.lookupR a m.rowLabels
    bIndex <- BM.lookupR b m.colLabels

    pure $ M.index' m.matrix (aIndex :. bIndex)

-- | Basic addition on two matrices. Returns Nothing if the provided matrices have different shapes.
add :: (Ord a, Ord b) => (c -> c -> c) -> LabelledMatrix a b c -> LabelledMatrix a b c -> Maybe (LabelledMatrix a b c)
add _ x y | assertAllWellFormed [x, y] = undefined
add addElems m1 m2 = undefined

-- | Basic addition on two matrices. Returns Nothing if the provided matrices have different shapes.
add' :: (Ord a, Ord b, Num c) => LabelledMatrix a b c -> LabelledMatrix a b c -> Maybe (LabelledMatrix a b c)
add' x y | assertAllWellFormed [x, y] = undefined
add' m1 m2
    | m1.rowLabels /= m2.rowLabels || m1.colLabels /= m2.colLabels = Nothing
    | otherwise = Just $ Matrix ((M.!+!) m1.matrix m2.matrix) m1.rowLabels m1.colLabels

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
    | m1.colLabels /= m2.rowLabels = Nothing
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
        (aRows, notARows) = T.both (Map.keysSet . BM.toMapR) $ BM.partition (\i _ -> i < aNumRows) m.rowLabels
        (aCols, notACols) = T.both (Map.keysSet . BM.toMapR) $ BM.partition (\i _ -> i < aNumCols) m.colLabels

        aNumRows = m.numRows `div` 2 + m.numRows `mod` 2
        aNumCols = m.numCols `div` 2 + m.numCols `mod` 2

        project' x y z = fromJust $ project x y z

{-

>>> True == False
False

-}

-- | Internal. Joins two disjoint matrices. Input may not be well formed, but the key sets of the maps must be disjoint otherwise an assertion will be thrown. Result may not be well-formed.
join :: (Ord a, Ord b) => LabelledMatrix a b c -> LabelledMatrix a b c -> Maybe (LabelledMatrix a b c)
join m1 m2 = undefined

-- TODO: THIS ASSUMES THE BIMAPS ARE INDENTICAL. This is quite different from just assuming
-- the domains are the same as the function did before - but this new assumption could lead
-- to performance improvements if it is true all the time (which i think it might be!)
-- If we encounter issues, check that this is not the cause of error.

-- TODO: I think we probably don't expose this as it seems a bit specific of a function?

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
    |    (fst $ BM.findMaxR a.rowLabels) < (fst $ BM.findMinR c.rowLabels)
      || (fst $ BM.findMaxR a.colLabels) < (fst $ BM.findMinR b.colLabels) = Nothing
    | otherwise = Just $ Matrix matrix rowLabels colLabels
    where
        matrix = append 2 (append 1 a.matrix b.matrix)
                          (append 1 c.matrix d.matrix)

        rowLabels = BM.fromAscPairList $ (++) (BM.toAscList a.rowLabels)
                                              (BM.toAscList $ BM.mapMonotonic (+ a.numRows) c.rowLabels)

        colLabels = BM.fromAscPairList $ (++) (BM.toAscList a.colLabels)
                                              (BM.toAscList $ BM.mapMonotonic (+ a.numCols) c.colLabels)

        append = ((M.computeAs M.B .) .) . M.append'

enumerate :: (Ord a) => S.Set a -> BM.Bimap M.Ix1 a
enumerate xs = BM.fromAscPairList $ zip [M.Ix1 0..] (S.toAscList xs)

{- | Returns true if the matrix satisfies a set of invariants that we wish
to maintain between operations.

A notable invariant that is not strictly necessary is that each mapping of
labels to indices must satisfy the rule that if `label i` < `label j` then
`index i` < `index j`. The reason for this invariant is that the underlying
matrix's addition and multiplication functions will pair elements up for
addition or multiplication based on their index - so a necessary precondition
of these methods is already that the exact same mapping of indices exist
in both LabelledMatricies. For example, we can't multiply `x` by `y` if
`(BM.!) x.colLabels 0 /= (BM.!) y.rowLabels 0`, even if
`x.colLabelSet == y.rowLabelSet`. If we don't include this invariant,
we have to check this on entry to addition or multiplication anyway,
and I feel the data structure is harder to use - the developer using this
data structure should probably not have to worry about this; details like
this should be kept internal. In short - if you have the correct domain
you should be ok. The developer shouldn't have to worry about the order too.
Technically you could have checks to make the matrices match order in
multiplication and addition for the best of both worlds - this could be
persued if necessary.
-}
isWellFormed :: (Eq a, Eq b) => LabelledMatrix a b c -> Bool
isWellFormed m
    -- Shape of matrix matches number of row and column labels.
    | m.numRows /= BM.size m.rowLabels || m.numCols /= BM.size m.colLabels = False
    -- No labels are mapped to indexes outside the bounds of the matrix
    | any (\v -> v < 0 || v >= m.numRows) (BM.keys m.rowLabels) = False
    | any (\v -> v < 0 || v >= m.numCols) (BM.keys m.colLabels) = False
    -- Entries in the bimap are strictly increasing in both arguments
    | BM.toAscList m.rowLabels /= map T.swap (BM.toAscListR m.rowLabels) = False
    | BM.toAscList m.colLabels /= map T.swap (BM.toAscListR m.colLabels) = False
    | otherwise = True

assertIsWellFormed :: (Eq a, Eq b) => LabelledMatrix a b c -> Bool
assertIsWellFormed x = assert (isWellFormed x) False

assertAllWellFormed :: (Eq a, Eq b, Foldable t) => t (LabelledMatrix a b c) -> Bool
assertAllWellFormed = any (\x -> assert (isWellFormed x) False)

