{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE OverloadedLists #-}

module Data
    ( asiaValuationsP1,
      asiaValuationsP2,
      asiaValuationsP3,
      asiaQueriesP1,
      asiaQueriesP2,
      asiaQueriesP3,
      asiaAnswersP1,
      asiaAnswersP2,
      asiaAnswersP3,
      AsiaVar (..),
      stringToAsiaVar,
      asiaFilepath,
      andesFilepath
    )
where

import           Data.Binary  (Binary)
import           GHC.Generics (Generic)

import           Bayesian

dataDirectory :: String
dataDirectory = "data/"

asiaFilepath :: String
asiaFilepath = dataDirectory ++ "asia.net"

andesFilepath :: String
andesFilepath = dataDirectory ++ "andes.net"


data AsiaVar = VisitToAsia | HasTuberculosis | Smoker | HasLungCancer
           | HasBronchitis | TuberculosisOrCancer | XRayResult | Dyspnea | XRayResultAndDyspnea
           deriving (Eq, Ord, Show, Generic, Binary)

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
asiaValuations :: [BayesValuation AsiaVar Bool]
asiaValuations =
    [ getRows $ Columns [VisitToAsia] [0.99, 0.01],
        getRows $ Columns [HasTuberculosis, VisitToAsia] [0.99, 0.95, 0.01, 0.05],
        getRows $ Columns [Smoker] [0.5, 0.5],
        getRows $ Columns [HasLungCancer, Smoker] [0.99, 0.9, 0.01, 0.1],
        getRows $ Columns [HasBronchitis, Smoker] [0.7, 0.4, 0.3, 0.6],
        getRows $ Columns [TuberculosisOrCancer, HasTuberculosis, HasLungCancer] [1, 0, 0, 0, 0, 1, 1, 1],
        getRows $ Columns [XRayResult, TuberculosisOrCancer] [0.95, 0.02, 0.05, 0.98],
        getRows $ Columns [Dyspnea, TuberculosisOrCancer, HasBronchitis] [0.9, 0.2, 0.3, 0.1, 0.1, 0.8, 0.7, 0.9]
    ]

asiaValuationsP1 :: [BayesValuation AsiaVar Bool]
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

asiaValuationsP2 :: [BayesValuation AsiaVar Bool]
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

asiaValuationsP3 :: [BayesValuation AsiaVar Bool]
asiaValuationsP3 = asiaValuations ++ [getRows $ Columns [XRayResultAndDyspnea, XRayResult, Dyspnea] [1, 1, 1, 0, 0, 0, 0, 1]]

asiaQueriesP3 :: [ProbabilityQuery AsiaVar Bool]
asiaQueriesP3 = map toProbabilityQuery [
        (
          [(XRayResultAndDyspnea, True)],
          [(VisitToAsia, True)]
        )
    ]

asiaAnswersP3 :: [Probability]
asiaAnswersP3 = [0.099]


