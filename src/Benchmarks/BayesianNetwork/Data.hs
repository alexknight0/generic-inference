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
    , asiaVariables
    , muninQueries
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
    , childFilepath
    , muninFilepath
    , genQuery
    , genQueries
    , genQueriesExact
    , Gen.sample
    , H.Gen
    , VariableFrames
    , genProblem
    , Problems (..)
    , NamedNet (..)
    )
where

import qualified LocalComputation.Instances.BayesianNetwork as BN
import           LocalComputation.ValuationAlgebra          (Binary, Generic,
                                                             NFData)

import qualified Benchmarks.Utils                           as U
import qualified Control.Monad.IO.Class                     as M
import qualified Data.Map                                   as M
import qualified Hedgehog                                   as H
import qualified Hedgehog.Gen                               as Gen
import qualified Hedgehog.Range                             as Range
import qualified LocalComputation.Utils                     as U
import qualified LocalComputation.ValuationAlgebra          as V
import qualified LocalComputation.ValuationAlgebra.Semiring as S

--------------------------------------------------------------------------------
-- Query Generation
--------------------------------------------------------------------------------
type VariableFrames a b = M.Map a (V.Domain b)

-- | Generates a variable assignment from a set of variables and their possible values.
-- If the requested `numVars` exceeds the number of variables in `vars` then simply
-- assigns as many as possible.
genVarAssignment :: (Ord a) => Int -> VariableFrames a b -> H.Gen (BN.VarAssignment a b)
genVarAssignment numVars _
    | numVars < 0           = U.assertError
genVarAssignment numVars vars = do
    randomSubMap <- fmap (M.fromList . take numVars) $ Gen.shuffle (M.toList vars)
    sequence $ M.map Gen.element randomSubMap

-- | Generates a random query using the given naturals as upper bounds on the number
-- of variables that can be found in the conditioned and conditional variables respectively.
genQuery :: (Ord a) => Int -> Int -> VariableFrames a b -> H.Gen (BN.Query a b)
genQuery maxConditioned maxConditional vars
    | maxConditioned <= 0 = U.assertError
    | maxConditional <  0 = U.assertError
    | length vars == 0    = U.assertError
genQuery maxConditioned maxConditional vars = do
    numConditioned <- Gen.int $ Range.constant 1 maxConditioned
    numConditional <- Gen.int $ Range.constant 0 maxConditional

    genQueryExact numConditioned numConditional vars

genQueryExact :: (Ord a) => Int -> Int -> VariableFrames a b -> H.Gen (BN.Query a b)
genQueryExact numConditioned numConditional vars
    | numConditioned <= 0 = U.assertError
    | numConditional <  0 = U.assertError
    | length vars == 0    = U.assertError
genQueryExact numConditioned numConditional vars = do
    conditioned <- genVarAssignment numConditioned vars
    conditional <- genVarAssignment numConditional (M.difference vars conditioned)

    pure $ BN.Query conditioned conditional

genQueries :: (Ord a) => BN.Network a b -> Int -> Int -> Int -> H.Gen ([BN.Query a b])
genQueries valuations = genQueries' variables
    where
        variables = foldr M.union M.empty $ map S.toFrames valuations

genQueries' :: (Ord a) => VariableFrames a b -> Int -> Int -> Int -> H.Gen ([BN.Query a b])
genQueries' _    numQueries _ _ | numQueries == 0 = U.assertError
genQueries' vars numQueries maxConditioned maxConditional = Gen.list (Range.singleton numQueries)
                                                                     (genQuery maxConditioned maxConditional vars)

genQueriesExact :: (Ord a) => BN.Network a b -> Int -> Int -> Int -> H.Gen ([BN.Query a b])
genQueriesExact valuations = genQueriesExact' variables
    where
        variables = foldr M.union M.empty $ map S.toFrames valuations

genQueriesExact' :: (Ord a) => VariableFrames a b -> Int -> Int -> Int -> H.Gen ([BN.Query a b])
genQueriesExact' _    numQueries _ _ | numQueries == 0 = U.assertError
genQueriesExact' vars numQueries numConditioned numConditional = Gen.list (Range.singleton numQueries)
                                                                          (genQueryExact numConditioned numConditional vars)

--------------------------------------------------------------------------------
-- Problem generation
--------------------------------------------------------------------------------
data Problems = Problems {
      name           :: String
    , net            :: NamedNet (BN.Network String String)
    , numProblems    :: Int
    , problems       :: [[BN.Query String String]]
    , numConditioned :: Int
    , numConditional :: Int
    , seed           :: Maybe Int
}

data NamedNet a = NamedNet {
      name :: String
    , net  :: a
}


genProblem :: (M.MonadIO m) => NamedNet (BN.Network String String) -> Int -> Int -> Int -> Int -> Int -> m Problems
genProblem net numProblems numQueries numConditioned numConditional originalSeed = do

    seeds <- U.sample originalSeed $ Gen.list (Range.singleton numProblems)
                                              (Gen.int (Range.linear 0 1000000000000))

    problems <- mapM (\seed -> U.sample seed $ genQueriesExact net.net numQueries numConditioned numConditional) seeds


    pure $ Problems {
          name           = "Basic"
        , net            = net
        , numProblems    = numProblems
        , problems       = problems
        , numConditioned = numConditioned
        , numConditional = numConditional
        , seed           = Just originalSeed
    }


--------------------------------------------------------------------------------
-- Hardcoded test cases
--------------------------------------------------------------------------------

dataDirectory :: String
dataDirectory = "src/Benchmarks/BayesianNetwork/Data/"

asiaFilepath :: String
asiaFilepath = dataDirectory ++ "asia.net"

alarmFilepath :: String
alarmFilepath = dataDirectory ++ "alarm.net"

childFilepath :: String
childFilepath = dataDirectory ++ "child.net"

muninFilepath :: String
muninFilepath = dataDirectory ++ "munin.net"

--------------------------------------------------------------------------------
-- Hardcoded asia data
--------------------------------------------------------------------------------

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

asiaVariables :: VariableFrames AsiaVar Bool
asiaVariables = M.fromList $ map (\x -> (x, [False, True])) [minAsiaP1 .. maxAsiaP1]

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
asiaQueriesP1 = map BN.unsafeToQuery [
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

-- | Queries that the baseline model can't handle, but the generic implementation can.
asiaQueriesP2 :: [BN.Query AsiaVar Bool]
asiaQueriesP2 = map BN.unsafeToQuery [
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
asiaQueriesP3 = map BN.unsafeToQuery [
        (
          [(XRayResultAndDyspnea, True)],
          [(VisitToAsia, True)]
        )
    ]

asiaAnswersP3 :: [BN.Probability]
asiaAnswersP3 = [0.09882268]

--------------------------------------------------------------------------------
-- Other hardcoded test cases
--------------------------------------------------------------------------------
muninQueries :: [BN.Query String String]
muninQueries = map BN.unsafeToQuery [
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

alarmQueries  :: [BN.Query String String]
alarmQueries = map BN.unsafeToQuery [
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
thesisExampleQuery = BN.unsafeToQuery ([(Name, True), (ID, True)], [])
