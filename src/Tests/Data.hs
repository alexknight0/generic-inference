{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE OverloadedLists #-}

module Tests.Data
    ( asiaValuationsP1
    , asiaValuationsP2
    , asiaValuationsP3
    , asiaQueriesP1
    , asiaQueriesP2
    , asiaQueriesP3
    , asiaAnswersP1
    , asiaAnswersP2
    , asiaAnswersP3
    , AsiaVar (..)
    , minAsiaP1
    , maxAsiaP1
    , stringToAsiaVar
    , asiaFilepath
    , andesFilepath
    , fourierP1Queries
    , fourierP1Samples
    , fourierP1Answers
    , fourierP2Queries
    , fourierP2Samples
    , fourierP2Answers
    , graphP1
    , graphQueriesP1
    , graphAnswersP1
    )
where

-- Typeclasses
import           Data.Binary                                 (Binary)
import           GHC.Generics                                (Generic)

import           Data.Complex                                (Complex ((:+)))
import           Numeric.Natural

import           Instances.BayesianNetwork
import qualified Instances.FastFourierTransform              as F
import           ValuationAlgebra.QuasiRegular.SemiringValue

import           Instances.ShortestPath.HackageVersion       (DistanceGraph)
import           Utils

dataDirectory :: String
dataDirectory = "data/"

asiaFilepath :: String
asiaFilepath = dataDirectory ++ "asia.net"

andesFilepath :: String
andesFilepath = dataDirectory ++ "andes.net"


-- | Variables (AKA nodes) of the asia example. XRayResultAndDyspnea appears only in P3.
-- If order is updated, also update minAsiaP1 and maxAsiaP1 to define the range of values
-- that a random value should be generated between.
data AsiaVar = VisitToAsia | HasTuberculosis | Smoker | HasLungCancer
           | HasBronchitis | TuberculosisOrCancer | XRayResult | Dyspnea | XRayResultAndDyspnea
           deriving (Eq, Ord, Show, Generic, Binary, Enum)

minAsiaP1 :: AsiaVar
minAsiaP1 = VisitToAsia
maxAsiaP1 :: AsiaVar
maxAsiaP1 = Dyspnea

stringToAsiaVar :: String -> AsiaVar
stringToAsiaVar "asia"   = VisitToAsia
stringToAsiaVar "tub"    = HasTuberculosis
stringToAsiaVar "smoke"  = Smoker
stringToAsiaVar "lung"   = HasLungCancer
stringToAsiaVar "bronc"  = HasBronchitis
stringToAsiaVar "either" = TuberculosisOrCancer
stringToAsiaVar "xray"   = XRayResult
stringToAsiaVar "dysp"   = Dyspnea
stringToAsiaVar _        = error "Unexpected string representing an asia var."


-- These match the ones used in NENOK and in https://online.bayesserver.com/
-- (Only difference from project proposal is smoker).
-- Note the probably values from https://online.bayesserver.com/ round to 1dp after %.
asiaValuations :: [([AsiaVar], [Probability])]
asiaValuations = [
        ([VisitToAsia], [0.99, 0.01]),
        ([HasTuberculosis, VisitToAsia], [0.99, 0.95, 0.01, 0.05]),
        ([Smoker], [0.5, 0.5]),
        ([HasLungCancer, Smoker], [0.99, 0.9, 0.01, 0.1]),
        ([HasBronchitis, Smoker], [0.7, 0.4, 0.3, 0.6]),
        ([TuberculosisOrCancer, HasTuberculosis, HasLungCancer], [1, 0, 0, 0, 0, 1, 1, 1]),
        ([XRayResult, TuberculosisOrCancer], [0.95, 0.02, 0.05, 0.98]),
        ([Dyspnea, TuberculosisOrCancer, HasBronchitis], [0.9, 0.2, 0.3, 0.1, 0.1, 0.8, 0.7, 0.9])
    ]

asiaValuationsP1 :: [([AsiaVar], [Probability])]
asiaValuationsP1 = asiaValuations

asiaQueriesP1 :: [ProbabilityQuery AsiaVar Bool]
asiaQueriesP1 = map toProbabilityQuery [
          (
           [(HasTuberculosis, True)],
           [(VisitToAsia, True)]
          )
        , (
           [(TuberculosisOrCancer, True)],
           [(VisitToAsia, True), (Smoker, False), (HasBronchitis, True)]
          )
        , (
            [(TuberculosisOrCancer, True)],
            [(VisitToAsia, True), (HasTuberculosis, True), (Smoker, False), (HasBronchitis, True)]
          )
        , (
            [(TuberculosisOrCancer, True)],
            [(VisitToAsia, True), (HasTuberculosis, False), (Smoker, False), (HasBronchitis, True)]
          )
        , (
            [(Dyspnea, False)],
            [(HasTuberculosis, True), (HasLungCancer, True)]
          )
    ]

asiaAnswersP1 :: [Probability]
asiaAnswersP1 = [0.05, 0.0595, 1, 0.01, 0.1855]

asiaValuationsP2 :: [([AsiaVar], [Probability])]
asiaValuationsP2 = asiaValuations

asiaQueriesP2 :: [ProbabilityQuery AsiaVar Bool]
asiaQueriesP2 = map toProbabilityQuery [
          (
            [(XRayResult, True), (Dyspnea, True)],
            [(VisitToAsia, True)]
          )
        , (
            [(XRayResult, True), (Dyspnea, True)],
            [(HasBronchitis, False), (HasLungCancer, True)]
          )
    ]

asiaAnswersP2 :: [Probability]
asiaAnswersP2 = [0.099, 0.686]

asiaValuationsP3 :: [([AsiaVar], [Probability])]
asiaValuationsP3 = asiaValuations ++ [([XRayResultAndDyspnea, XRayResult, Dyspnea], [1, 1, 1, 0, 0, 0, 0, 1])]

asiaQueriesP3 :: [ProbabilityQuery AsiaVar Bool]
asiaQueriesP3 = map toProbabilityQuery [
        (
          [(XRayResultAndDyspnea, True)],
          [(VisitToAsia, True)]
        )
    ]

asiaAnswersP3 :: [Probability]
asiaAnswersP3 = [0.099]

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

{- | Example graph used for a shortest path problem.

Source: https://www.geeksforgeeks.org/dsa/dijkstras-algorithm-for-adjacency-list-representation-greedy-algo-8/
-}
graphP1 :: (Eq a, Num a) => DistanceGraph Integer a
graphP1 = assert' isValidGraph [ [((0, 0), 0), ((0, 1), 4), ((0, 7), 8)]
          , [((1, 1), 0), ((1, 0), 4), ((1, 2), 8), ((1, 7), 11)]
          , [((2, 2), 0), ((2, 1), 8), ((2, 3), 7), ((2, 5), 4), ((2, 8), 2)]
          , [((3, 3), 0), ((3, 2), 7), ((3, 4), 9), ((3, 5), 14)]
          , [((4, 4), 0), ((4, 3), 9), ((4, 5), 10)]
          , [((5, 5), 0), ((5, 2), 4), ((5, 3), 14), ((5, 4), 10), ((5, 6), 2)]
          , [((6, 6), 0), ((6, 5), 2), ((6, 7), 1), ((6, 8), 6)]
          , [((7, 7), 0), ((7, 0), 8), ((7, 1), 11), ((7, 6), 1), ((7, 8), 7)]
          , [((8, 8), 0), ((8, 2), 2), ((8, 6), 6), ((8, 7), 7)]
        ]

isValidGraph :: (Eq a, Eq b) => DistanceGraph a b -> Bool
isValidGraph xs = all (\((x, y), c) -> ((y, x), c) `elem` xs') xs'
    where
        xs' = concat xs

graphQueriesP1 :: ([Integer], Integer)
graphQueriesP1 = ([1
                , 2
                , 3
                , 4
                , 5
                , 6
                , 7
                , 8
            ], 0)

graphAnswersP1 :: [TropicalSemiringValue]
graphAnswersP1 = [4
                , 12
                , 19
                , 21
                , 11
                , 9
                , 8
                , 14
            ]


