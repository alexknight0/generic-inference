module PrebuiltBayesian
    ( asiaQuery )
where

import           Data                                 (AsiaVar (Dyspnea, HasBronchitis, HasLungCancer, HasTuberculosis, Smoker, TuberculosisOrCancer, VisitToAsia, VisitToAsia, XRayResult, XRayResultAndDyspnea))
import           Numeric.Probability.Distribution     ((?=<<), (??))
import           Numeric.Probability.Example.Bayesian (PState, Probability,
                                                       STrans, State, bin,
                                                       event, happens, network,
                                                       source, SPred)
import           Utils                                (findAssertSingleMatch)

doesNotHappen :: Eq a => SPred a
doesNotHappen x y = not (x `elem` y)

asiaQuery :: [Double]
asiaQuery = map fromRational [
          happens VisitToAsia ?? asiaNet
        , happens HasTuberculosis ?? asiaNet
        , happens HasTuberculosis ?? doesNotHappen VisitToAsia ?=<< asiaNet
        , happens HasTuberculosis ?? happens VisitToAsia ?=<< asiaNet
        , happens HasTuberculosis ?? happens VisitToAsia ?=<< happens Smoker ?=<< asiaNet
        , happens TuberculosisOrCancer ?? asiaNet
        , happens TuberculosisOrCancer ?? happens VisitToAsia ?=<< asiaNet
        , happens TuberculosisOrCancer ?? happens VisitToAsia ?=<< happens Smoker ?=<< happens XRayResult ?=<< asiaNet
    ]

asiaNet :: PState AsiaVar
asiaNet = network [
        source 0.01 VisitToAsia,
        potential HasTuberculosis [VisitToAsia] [0.01, 0.05],
        source 0.5 Smoker,
        potential HasLungCancer [Smoker] [0.01, 0.1],
        potential HasBronchitis [Smoker] [0.3, 0.6],
        potential TuberculosisOrCancer [HasTuberculosis, HasLungCancer] [0, 1, 1, 1],
        potential XRayResult [TuberculosisOrCancer] [0.05, 0.98],
        potential Dyspnea [TuberculosisOrCancer, HasBronchitis] [0.1, 0.8, 0.7, 0.9]
    ]

{- | A probability potential.

Probability values are only entered for the half of the table
that considers the conditioned variable is true. The probability
values are ordered from False to True, i.e.

X   |   A   B   |   P
-----------------------
T   |   F   F   |   0
T   |   F   T   |   1
T   |   T   F   |   2
T   |   T   T   |   3


X is the conditioned variable, A and B are the conditional variables
and P is probability. A is before B inside the conditional variables
list, and probability values inside the the table are the index of their
position inside the probabilities list.
-}
potential :: Eq a => a -> [a] -> [Probability] -> STrans a
potential conditionedV conditionalVs ps s = event p conditionedV s
    where
        (p, _) = findAssertSingleMatch snd events

        events :: [(Probability, Bool)]
        events = zip ps (map (hasVariableSetOccured s) $ vPermutations conditionalVs)




hasVariableSetOccured :: Eq a => State a -> [(a, Bool)] -> Bool
hasVariableSetOccured s xs = all f xs
    where
        f (var, varIsTrue)
            | varIsTrue = elem var s
            | otherwise = not (elem var s)



vPermutations :: [a] -> [[(a, Bool)]]
vPermutations [] = [[]]
vPermutations (v : vs) = [(v, value) : rest | value <- [False, True], rest <- vPermutations vs]

