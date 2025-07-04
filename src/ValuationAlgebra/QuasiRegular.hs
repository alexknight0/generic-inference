
module ValuationAlgebra.QuasiRegular
    ( QuasiRegularSemiringValue (add, multiply, quasiInverse)
    )
where

import           Control.Exception                           (assert)
import           Data.Maybe                                  (fromJust)
import qualified Data.Set                                    as S
import qualified LabelledMatrix                              as M
import           Utils
import           ValuationAlgebra
import           ValuationAlgebra.QuasiRegular.SemiringValue

data QuasiRegularValuation c a b = Valuation (M.LabelledMatrix a a c) (M.LabelledMatrix a () c) | Identity



create :: (Eq a) => M.LabelledMatrix a a c -> M.LabelledMatrix a () c -> Maybe (QuasiRegularValuation c a b)
create m b
    | isWellFormed (Valuation m b) = Just (Valuation m b)
    | otherwise = Nothing

isWellFormed :: (Eq a) => QuasiRegularValuation c a b -> Bool
isWellFormed Identity = True
isWellFormed (Valuation m b) = (M.isSquare m) && ((fst $ M.domain m) == (fst $ M.domain b))

assertIsWellFormed :: (Eq a) => QuasiRegularValuation c a b -> Bool
assertIsWellFormed x = assert (isWellFormed x) False

errorInvalidStructure :: a
errorInvalidStructure = error "QuasiRegularValuation is not well formed."

instance (QuasiRegularSemiringValue c) => Valuation (QuasiRegularValuation c) where
    label x | assertIsWellFormed x = undefined
    label Identity        = S.empty
    label (Valuation _ b) = fst (M.domain b)

    combine x y | assertIsWellFormed x || assertIsWellFormed y = undefined
    combine Identity x = x
    combine x Identity = x
    combine v1 v2 = valuationAdd (extension v1 sUnionT) (extension v2 sUnionT)
        where
            sUnionT = S.union (label v1) (label v2)

    project x _ | assertIsWellFormed x = undefined
    project Identity _ = Identity
    project (Valuation m b) t = fromJust $ create newM newB
        where
            newM = matrixAdd (matrixProject m t t)
                             (matrixMultiply (x)
                                             (matrixProject m sMinusT t))
            newB = matrixAdd (matrixProject b sMinusT (S.singleton ()))
                             (matrixMultiply (x)
                                             (matrixProject b sMinusT (S.singleton ())))

            x = matrixMultiply (matrixProject m t sMinusT)
                               (matrixQuasiInverse $ matrixProject m sMinusT sMinusT)
            s = fst $ M.domain m
            sMinusT = S.difference s t

    identity = Identity

-- | Adds two valuations. Unsafe.
valuationAdd :: (Eq a, Ord a, QuasiRegularSemiringValue c) => QuasiRegularValuation c a b -> QuasiRegularValuation c a b -> QuasiRegularValuation c a b
valuationAdd x y | assertIsWellFormed x || assertIsWellFormed y = undefined
valuationAdd (Valuation m1 b1) (Valuation m2 b2) = fromJust $ create (matrixAdd m1 m2) (matrixAdd b1 b2)
valuationAdd _ _ = error "Not implemented error."  -- Not 100% certain on how to handle identity elements, but never called anyway.

-- | Extends a valuation. Unsafe.
extension :: (Ord a, QuasiRegularSemiringValue c) => QuasiRegularValuation c a b -> S.Set a -> QuasiRegularValuation c a b
extension x _ | assertIsWellFormed x = undefined
extension Identity _ = Identity
extension (Valuation m b) t = fromJust $ create (fromJust $ M.extension m t t zero) (fromJust $ M.extension b t (S.singleton ()) zero)

matrixQuasiInverse :: (Ord a, QuasiRegularSemiringValue c) => M.LabelledMatrix a a c -> M.LabelledMatrix a a c
matrixQuasiInverse = fromJust . M.quasiInverse

matrixProject :: (Ord a, Ord b) => M.LabelledMatrix a b c -> S.Set a -> S.Set b -> M.LabelledMatrix a b c
matrixProject = ((fromJust .) .) . M.project

matrixAdd :: (Ord a, Ord b, QuasiRegularSemiringValue c) => M.LabelledMatrix a b c -> M.LabelledMatrix a b c -> M.LabelledMatrix a b c
matrixAdd = (fromJust .) . M.add add

matrixMultiply :: (Ord a, Ord b, Ord c, QuasiRegularSemiringValue d) => M.LabelledMatrix a b d -> M.LabelledMatrix b c d -> M.LabelledMatrix a c d
matrixMultiply = (fromJust .) . M.multiply add multiply

matrixMultiplys :: (Foldable t, Functor t, Ord a, QuasiRegularSemiringValue c) => t (M.LabelledMatrix a a c) -> M.LabelledMatrix a a c
matrixMultiplys = fromJust . M.multiplys add multiply








