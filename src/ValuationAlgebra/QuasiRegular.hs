{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module ValuationAlgebra.QuasiRegular
    ( QuasiRegularSemiringValue (quasiInverse)
    , SemiringValue (add, multiply, zero, one)
    , QuasiRegularValuation
    , create
    , solution
    , TropicalSemiringValue (T)
    )
where

import           Control.Exception                           (assert)
import           Data.Maybe                                  (fromJust)
import qualified Data.Set                                    as S
import qualified LabelledMatrix                              as M
import           Utils
import           ValuationAlgebra
import           ValuationAlgebra.QuasiRegular.SemiringValue
import           ValuationAlgebra.SemiringValue

-- Typeclasses
import           Control.DeepSeq                             (NFData)
import           Data.Binary                                 (Binary)
import           Debug.Trace                                 (trace)
import           GHC.Generics                                (Generic)

data QuasiRegularValuation c a b = Valuation (M.LabelledMatrix a a c) (M.LabelledMatrix a () c) | Identity (Domain a) deriving (Binary, NFData, Ord, Eq, Generic, Read, Show)

create :: (Eq a) => M.LabelledMatrix a a c -> M.LabelledMatrix a () c -> Maybe (QuasiRegularValuation c a b)
create m b
    | isWellFormed (Valuation m b) = Just (Valuation m b)
    | otherwise = Nothing

instance (Show c, QuasiRegularSemiringValue c) => Valuation (QuasiRegularValuation c) where
    label x | assertIsWellFormed x = undefined
    label (Identity d)    = d
    label (Valuation _ b) = fst (M.domain b)

    combine x y | assertIsWellFormed x || assertIsWellFormed y = undefined
    combine i@(Identity _) x = x
    combine x i@(Identity _) = x
    combine v1 v2 = valuationAdd (extension v1 sUnionT) (extension v2 sUnionT)
        where
            sUnionT = S.union (label v1) (label v2)

    project x _ | assertIsWellFormed x = undefined
    project (Identity _) newD = Identity newD
    project (Valuation m b) t = fromJust $ create newM newB
        where
            newM = matrixAdd (matrixProject (show t) m t t)
                             (matrixMultiply (x)
                                             (matrixProject "newM2" m sMinusT t))
            newB = matrixAdd (matrixProject "newB1" b t (S.singleton ()))
                             (matrixMultiply (x)
                                             (matrixProject "newB2" b sMinusT (S.singleton ())))

            x = matrixMultiply (matrixProject "newX1" m t sMinusT)
                               (matrixQuasiInverse $ matrixProject "newX2" m sMinusT sMinusT)
            s = fst $ M.domain m
            sMinusT = S.difference s t

    identity d = Identity d

-- | Returns a product useful for the solution of fixpoint systems. Detailed page 367 of "Generic Inference" (Pouly & Kohlas, 2012)
solution :: (Show a, Ord a, QuasiRegularSemiringValue c) => QuasiRegularValuation c a b -> M.LabelledMatrix a () c
solution (Identity _)    = error "'solution' called on identity valuation."
solution (Valuation m b) = matrixMultiply (matrixQuasiInverse m) b

-- | Adds two valuations. Unsafe.
valuationAdd :: (Ord a, QuasiRegularSemiringValue c) => QuasiRegularValuation c a b -> QuasiRegularValuation c a b -> QuasiRegularValuation c a b
valuationAdd x y | assertIsWellFormed x || assertIsWellFormed y = undefined
valuationAdd (Valuation m1 b1) (Valuation m2 b2) = fromJust $ create (matrixAdd m1 m2) (matrixAdd b1 b2)
valuationAdd _ _ = error "Not implemented error."  -- Not 100% certain on how to handle identity elements, but never called anyway.

-- | Extends a valuation. Unsafe.
extension :: (Ord a, Show a, QuasiRegularSemiringValue c) => QuasiRegularValuation c a b -> S.Set a -> QuasiRegularValuation c a b
extension x _ | assertIsWellFormed x = undefined
extension (Identity _) d = Identity d
extension (Valuation m b) t = fromJust $ create (fromJust $ M.extension m t t zero) (fromJust $ M.extension b t (S.singleton ()) zero)

matrixQuasiInverse :: (Show a, Ord a, QuasiRegularSemiringValue c) => M.LabelledMatrix a a c -> M.LabelledMatrix a a c
matrixQuasiInverse = fromJust . M.quasiInverse

matrixProject :: (Show a, Show b, Ord a, Ord b, SemiringValue c) => String -> M.LabelledMatrix a b c -> S.Set a -> S.Set b -> M.LabelledMatrix a b c
matrixProject x y z w
    | Nothing <- M.project y z w = fromJust $ M.extension y z w zero -- error x
    | otherwise = fromJust $ M.project y z w

matrixAdd :: (Ord a, Ord b, QuasiRegularSemiringValue c) => M.LabelledMatrix a b c -> M.LabelledMatrix a b c -> M.LabelledMatrix a b c
matrixAdd = (fromJust .) . M.add add

matrixMultiply :: (Ord a, Ord b, Ord c, QuasiRegularSemiringValue d) => M.LabelledMatrix a b d -> M.LabelledMatrix b c d -> M.LabelledMatrix a c d
matrixMultiply = (fromJust .) . M.multiply add multiply zero

matrixMultiplys :: (Foldable t, Functor t, Ord a, QuasiRegularSemiringValue c) => t (M.LabelledMatrix a a c) -> M.LabelledMatrix a a c
matrixMultiplys = fromJust . M.multiplys add multiply zero

isWellFormed :: (Eq a) => QuasiRegularValuation c a b -> Bool
isWellFormed (Identity _) = True
isWellFormed (Valuation m b) = (M.isSquare m) && ((fst $ M.domain m) == (fst $ M.domain b)) && M.isWellFormed m && M.isWellFormed b

assertIsWellFormed :: (Eq a) => QuasiRegularValuation c a b -> Bool
assertIsWellFormed x = assert (isWellFormed x) False

