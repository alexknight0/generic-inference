{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE OverloadedLists #-}

{- | Test cases and test case generation.

Where answers have been manually entered, the values have been obtained
using the 'R' script located in 'dataDirectory'.
-}
module Benchmarks.BayesianNetwork.Data
    ( asiaValuationsP1
    , asiaValuationsP2
    , asiaValuationsP3
    , asiaQueriesP1
    , asiaQueriesP2
    , asiaQueriesP3
    , asiaAnswersP1
    , asiaAnswersP2
    , asiaAnswersP3
    , muninQueries
    , muninAnswers
    , alarmQueries
    , alarmAnswers
    , thesisExampleQuery
    , thesisExampleValuations
    , AsiaVar (..)
    , minAsiaP1
    , maxAsiaP1
    , stringToAsiaVar
    , asiaFilepath
    , alarmFilepath
    , muninFilepath
    , genQueries
    , genQueriesExact
    , Gen.sample
    , H.Gen
    )
where

import qualified LocalComputation.Instances.BayesianNetwork as BN
import           LocalComputation.ValuationAlgebra          (Binary, Generic,
                                                             NFData)

import qualified Data.Map                                   as M
import qualified Hedgehog                                   as H
import qualified Hedgehog.Gen                               as Gen
import qualified Hedgehog.Range                             as Range
import qualified LocalComputation.Utils                     as U
import qualified LocalComputation.ValuationAlgebra          as V
import qualified LocalComputation.ValuationAlgebra.Semiring as S

--------------------------------------------------------------------------------
-- Test case generation
--------------------------------------------------------------------------------

type Variables a b = M.Map a (V.Domain b)

-- | Generates a variable assignment from a set of variables and their possible values.
-- If the requested `numVars` exceeds the number of variables in `vars` then simply
-- assigns as many as possible.
genVarAssignment :: Int -> Variables String String -> H.Gen (BN.VarAssignment String String)
genVarAssignment numVars _
    | numVars < 0           = U.assertError
genVarAssignment numVars vars = do
    randomSubMap <- fmap (M.fromList . take numVars) $ Gen.shuffle (M.toList vars)
    sequence $ M.map (\possibleValues -> Gen.element possibleValues)
                     randomSubMap

-- | Generates a random query using the given naturals as upper bounds on the number
-- of variables that can be found in the conditioned and conditional variables respectively.
genQuery :: Int -> Int -> Variables String String -> H.Gen (BN.Query String String)
genQuery maxConditioned maxConditional vars
    | maxConditioned <= 0 = U.assertError
    | maxConditional <  0 = U.assertError
    | length vars == 0    = U.assertError
genQuery maxConditioned maxConditional vars = do
    numConditioned <- Gen.int $ Range.constant 1 maxConditioned
    numConditional <- Gen.int $ Range.constant 0 maxConditional

    genQueryExact numConditioned numConditional vars


genQueryExact :: Int -> Int -> Variables String String -> H.Gen (BN.Query String String)
genQueryExact numConditioned numConditional vars
    | numConditioned <= 0 = U.assertError
    | numConditional <  0 = U.assertError
    | length vars == 0    = U.assertError
genQueryExact numConditioned numConditional vars = do
    conditioned <- genVarAssignment numConditioned vars
    conditional <- genVarAssignment numConditional (M.difference vars conditioned)

    pure $ BN.Query conditioned conditional


genQueries :: BN.Network String String -> Int -> Int -> Int -> H.Gen ([BN.Query String String])
genQueries valuations = genQueries' variables
    where
        variables = foldr M.union M.empty $ map S.toFrames valuations

genQueries' :: Variables String String -> Int -> Int -> Int -> H.Gen ([BN.Query String String])
genQueries' _    numQueries _ _ | numQueries == 0 = U.assertError
genQueries' vars numQueries maxConditioned maxConditional = Gen.list (Range.singleton numQueries)
                                                                     (genQuery maxConditioned maxConditional vars)

genQueriesExact :: BN.Network String String -> Int -> Int -> Int -> H.Gen ([BN.Query String String])
genQueriesExact valuations = genQueriesExact' variables
    where
        variables = foldr M.union M.empty $ map S.toFrames valuations

genQueriesExact' :: Variables String String -> Int -> Int -> Int -> H.Gen ([BN.Query String String])
genQueriesExact' _    numQueries _ _ | numQueries == 0 = U.assertError
genQueriesExact' vars numQueries numConditioned numConditional = Gen.list (Range.singleton numQueries)
                                                                          (genQueryExact numConditioned numConditional vars)


--------------------------------------------------------------------------------
-- Manual test cases
--------------------------------------------------------------------------------

dataDirectory :: String
dataDirectory = "src/Benchmarks/BayesianNetwork/Data/"

asiaFilepath :: String
asiaFilepath = dataDirectory ++ "asia.net"

alarmFilepath :: String
alarmFilepath = dataDirectory ++ "alarm.net"

muninFilepath :: String
muninFilepath = dataDirectory ++ "munin.net"

-- | Variables (AKA nodes) of the asia example. XRayResultAndDyspnea appears only in P3.
-- If order is updated, also update minAsiaP1 and maxAsiaP1 to define the range of values
-- that a random value should be generated between.
data AsiaVar = VisitToAsia | HasTuberculosis | Smoker | HasLungCancer
           | HasBronchitis | TuberculosisOrCancer | XRayResult | Dyspnea | XRayResultAndDyspnea
           deriving (Eq, Ord, Show, Generic, Binary, Enum, NFData)

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


-- These valuations match the commonly used probability potentials, which slightly differ from
-- the example displayed in the thesis report in the values for 'Smoker'.
asiaValuations :: [([AsiaVar], [BN.Probability])]
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

asiaValuationsP1 :: [([AsiaVar], [BN.Probability])]
asiaValuationsP1 = asiaValuations

asiaQueriesP1 :: [BN.Query AsiaVar Bool]
asiaQueriesP1 = map BN.toQueryA [
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

asiaAnswersP1 :: [BN.Probability]
asiaAnswersP1 = [0.05, 0.0595, 1, 0.01, 0.1854545]

asiaValuationsP2 :: [([AsiaVar], [BN.Probability])]
asiaValuationsP2 = asiaValuations

asiaQueriesP2 :: [BN.Query AsiaVar Bool]
asiaQueriesP2 = map BN.toQueryA [
          (
            [(XRayResult, True), (Dyspnea, True)],
            [(VisitToAsia, True)]
          )
        , (
            [(XRayResult, True), (Dyspnea, True)],
            [(HasBronchitis, False), (HasLungCancer, True)]
          )
        , (
            [(XRayResult, True), (Dyspnea, True)],
            [(HasBronchitis, False), (HasLungCancer, True), (Dyspnea, True)]
          )
        , (
            [(XRayResult, True), (Dyspnea, True)],
            [(HasBronchitis, False), (HasLungCancer, True), (XRayResult, True)]
          )
        , (
            [(XRayResult, True), (Dyspnea, True)],
            [(HasBronchitis, False), (HasLungCancer, True), (XRayResult, True), (Dyspnea, True)]
          )
        , (
            [(XRayResult, True), (Dyspnea, True)],
            [(HasBronchitis, False), (HasLungCancer, True), (XRayResult, True), (Dyspnea, False)]
          )
    ]

asiaAnswersP2 :: [BN.Probability]
asiaAnswersP2 = [0.09882268, 0.686, 0.98, 0.7, 1, 0]

asiaValuationsP3 :: [([AsiaVar], [BN.Probability])]
asiaValuationsP3 = asiaValuations ++ [([XRayResultAndDyspnea, XRayResult, Dyspnea], [1, 1, 1, 0, 0, 0, 0, 1])]

asiaQueriesP3 :: [BN.Query AsiaVar Bool]
asiaQueriesP3 = map BN.toQueryA [
        (
          [(XRayResultAndDyspnea, True)],
          [(VisitToAsia, True)]
        )
    ]

asiaAnswersP3 :: [BN.Probability]
asiaAnswersP3 = [0.09882268]

muninQueries :: [BN.Query String String]
muninQueries = map BN.toQueryA [
        -- Pretty sure we want underscores instead of '.'s here. Also may need to replace the dashes? idk. Maybe not.
          (
            [("R.MEDD2.DISP-WD", "NO")],
            []
          )
        , (
            [("R.MEDD2.DISP-WD", "NO")],
            [("R.DIFFN.MEDD2.DIFSLOW", "NO")]
          )
    ]

-- TODO: THESE ARE WRONG. NEED TO BE UPDATED.
muninAnswers :: [BN.Probability]
muninAnswers = [0.864, 0.874]

alarmQueries  :: [BN.Query String String]
alarmQueries = map BN.toQueryA [
          (
            [("HRBP", "LOW")],
            []
          )
        , (
            [("HRBP", "LOW")],
            [("HR", "LOW")]
          )
        , (
            [("ARTCO2", "LOW")],
            []
          )
        , (
            [("ARTCO2", "LOW")],
            [("EXPCO2", "LOW")]
          )
    ]

alarmAnswers :: [BN.Probability]
alarmAnswers = [0.17602606, 0.429, 0.18015254, 0.18262807]

data ThesisExample = ID | Name | Age | Birthplace deriving (Eq, Ord, Show, Binary, Generic, NFData)

thesisExampleValuations :: [([ThesisExample], [BN.Probability])]
thesisExampleValuations = [
        ([Name, Age, Birthplace], [0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5]),
        ([Name, ID], [0.5, 0.5, 0.5, 0.5])
    ]

thesisExampleQuery :: BN.Query ThesisExample Bool
thesisExampleQuery = BN.toQueryA ([(Name, True), (ID, True)], [])
