module Benchmarks.FastFourierTransform.Data
    ( fourierP1Queries
    , fourierP1Samples
    , fourierP1Answers
    , fourierP2Queries
    , fourierP2Samples
    , fourierP2Answers
    )
where

import           Data.Complex                                    (Complex ((:+)))
import           Numeric.Natural

import qualified LocalComputation.Instances.FastFourierTransform as F

fourierP1Queries :: [Natural]
fourierP1Queries = zipWith const [0..] fourierP1Samples

fourierP1Samples :: [F.FourierComplex]
fourierP1Samples = map F.FourierComplex [1 :+ 0, 2 :+ 0, 3 :+ 0, 4 :+ 0]

fourierP1Answers :: [F.FourierComplex]
fourierP1Answers = map F.FourierComplex [10 :+ 0, (negate 2) :+ 2, (negate 2) :+ 0, (negate 2) :+ (negate 2)]

fourierP2Queries :: [Natural]
fourierP2Queries = zipWith const [0..] fourierP2Samples

fourierP2Samples :: [F.FourierComplex]
fourierP2Samples = map F.FourierComplex [1 :+ 0, 4 :+ 0]

fourierP2Answers :: [F.FourierComplex]
fourierP2Answers = map F.FourierComplex [5 :+ 0, (negate 3) :+ 0]

