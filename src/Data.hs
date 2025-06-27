{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE OverloadedLists #-}

module Data
    ( AsiaVar, AsiaValue, asiaDefaultQueries, asiaDefaultValuations, asiaMultipleConditionedVarsQueries, asiaMultipleConditionedVarsValuations, stringToAsiaVar, asiaDefaultAnswers)
where

import           Data.Binary      (Binary)
import qualified Data.Map         as M
import           GHC.Generics     (Generic)

import           Bayesian
import           Utils
import           ValuationAlgebra


data AsiaVar = VisitToAsia | HasTuberculosis | Smoker | HasLungCancer
           | HasBronchitis | TuberculosisOrCancer | XRayResult | Dyspnea | XRayResultAndDyspnea
           deriving (Eq, Ord, Show, Generic, Binary)

type AsiaValue = Bool

stringToAsiaVar :: String -> AsiaVar
stringToAsiaVar "asia" = VisitToAsia
stringToAsiaVar "tub" = HasTuberculosis
stringToAsiaVar "smoke" = Smoker
stringToAsiaVar "lung" = HasLungCancer
stringToAsiaVar "bronc" = HasBronchitis
stringToAsiaVar "either" = TuberculosisOrCancer
stringToAsiaVar "xray" = XRayResult
stringToAsiaVar "dysp" = Dyspnea
stringToAsiaVar _ = error "Unexpected string representing an asia var."

-- These match the ones used in NENOK and in https://online.bayesserver.com/
-- (Only difference from project proposal is smoker).
-- Note the probably values from https://online.bayesserver.com/ round to 1dp after %.
asiaDefaultValuations :: [BayesValuation AsiaVar AsiaValue]
asiaDefaultValuations =
    [ getRows $ Columns [VisitToAsia] [0.99, 0.01],
        getRows $ Columns [HasTuberculosis, VisitToAsia] [0.99, 0.95, 0.01, 0.05],
        getRows $ Columns [Smoker] [0.5, 0.5],
        getRows $ Columns [HasLungCancer, Smoker] [0.99, 0.9, 0.01, 0.1],
        getRows $ Columns [HasBronchitis, Smoker] [0.7, 0.4, 0.3, 0.6],
        getRows $ Columns [TuberculosisOrCancer, HasTuberculosis, HasLungCancer] [1, 0, 0, 0, 0, 1, 1, 1],
        getRows $ Columns [XRayResult, TuberculosisOrCancer] [0.95, 0.02, 0.05, 0.98],
        getRows $ Columns [Dyspnea, TuberculosisOrCancer, HasBronchitis] [0.9, 0.2, 0.3, 0.1, 0.1, 0.8, 0.7, 0.9]
    ]

asiaDefaultQueries :: [ProbabilityQuery AsiaVar AsiaValue]
asiaDefaultQueries = [
          (M.singleton HasTuberculosis True, M.singleton VisitToAsia True)
        , (M.singleton TuberculosisOrCancer True, fromListAssertDisjoint [(VisitToAsia, True), (Smoker, False), (HasBronchitis, True)])
        , (M.singleton TuberculosisOrCancer True, fromListAssertDisjoint [(VisitToAsia, True), (HasTuberculosis, True), (Smoker, False), (HasBronchitis, True)])
        , (M.singleton TuberculosisOrCancer True, fromListAssertDisjoint [(VisitToAsia, True), (HasTuberculosis, False), (Smoker, False), (HasBronchitis, True)])
        , (M.singleton Dyspnea False, fromListAssertDisjoint [(HasTuberculosis, True), (HasLungCancer, True)])
        , (fromListAssertDisjoint [(XRayResult, True), (Dyspnea, True)], fromListAssertDisjoint [(VisitToAsia, True)])
        , (fromListAssertDisjoint [(XRayResult, True), (Dyspnea, True)], fromListAssertDisjoint [(HasBronchitis, False), (HasLungCancer, True)])
    ]
asiaDefaultAnswers :: [Probability]
asiaDefaultAnswers = [0.05, 0.0595, 1, 0.01, 0.1855, 0.099, 0.686]

asiaMultipleConditionedVarsValuations :: [BayesValuation AsiaVar AsiaValue]
asiaMultipleConditionedVarsValuations =
    [ getRows $ Columns [VisitToAsia] [0.99, 0.01],
        getRows $ Columns [HasTuberculosis, VisitToAsia] [0.99, 0.95, 0.01, 0.05],
        getRows $ Columns [Smoker] [0.5, 0.5],
        getRows $ Columns [HasLungCancer, Smoker] [0.99, 0.9, 0.01, 0.1],
        getRows $ Columns [HasBronchitis, Smoker] [0.7, 0.4, 0.3, 0.6],
        getRows $ Columns [TuberculosisOrCancer, HasTuberculosis, HasLungCancer] [1, 0, 0, 0, 0, 1, 1, 1],
        getRows $ Columns [XRayResult, TuberculosisOrCancer] [0.95, 0.02, 0.05, 0.98],
        getRows $ Columns [Dyspnea, TuberculosisOrCancer, HasBronchitis] [0.9, 0.2, 0.3, 0.1, 0.1, 0.8, 0.7, 0.9],
        getRows $ Columns [XRayResultAndDyspnea, XRayResult, Dyspnea] [1, 1, 1, 0, 0, 0, 0, 1]
    ]

asiaMultipleConditionedVarsQueries :: [ProbabilityQuery AsiaVar AsiaValue]
asiaMultipleConditionedVarsQueries = [
        (fromListAssertDisjoint [(XRayResultAndDyspnea, True)], fromListAssertDisjoint [(VisitToAsia, True)])
    ]



