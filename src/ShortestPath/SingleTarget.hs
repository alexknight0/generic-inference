{-# LANGUAGE FlexibleContexts #-}
module ShortestPath.SingleTarget
    ( answerQuery
    , answerQueries
    , DistanceMap
    )
where

import           Control.Distributed.Process                 (Process)
import           Data.Maybe                                  (fromJust)
import qualified Data.Set                                    as S

import qualified LabelledMatrix                              as M
import           ShenoyShafer                                (answerQueriesM,
                                                              answerQueryM)
import           ValuationAlgebra
import           ValuationAlgebra.QuasiRegular               (QuasiRegularValuation,
                                                              create, solution)
import           ValuationAlgebra.QuasiRegular.SemiringValue

-- Typeclasses
import           Control.Distributed.Process.Serializable    (Serializable)
import           Data.Binary                                 (Binary)
import           Type.Reflection                             (Typeable)

type DistanceMap a = M.LabelledMatrix a a TropicalSemiringValue
type DistanceVector a = M.LabelledMatrix a () TropicalSemiringValue
type Knowledgebase a = [QuasiRegularValuation TropicalSemiringValue a ()]
type Query a = (a, a)

-- TODO: could make shortest path to self zero by adding an
-- extensionWith function and then using it.
makeMapSquare :: (Ord a, Show a) => DistanceMap a -> DistanceMap a
makeMapSquare m = fromJust $ M.extension m newD newD zero
    where
        (d1, d2) = M.domain m
        newD = S.union d1 d2

knowledgeBase :: (Ord a, Show a) => [DistanceMap a] -> a -> Knowledgebase a
knowledgeBase xs target = map f xs
    where
        f m = fromJust $ create m' b
            where
                m' = makeMapSquare m
                b = M.fromList [((a, ()), if a == target then one else zero) | a <- S.toList $ fst (M.domain m')]

-- | Retuns an entry from a distance map. Unsafe.
getDistance :: (Ord a, Show a) => DistanceVector a -> Query a -> TropicalSemiringValue
getDistance m (source, _) = fromJust $ M.find (source, ()) m

answerQuery :: (Show a, Typeable a, Binary a,  Binary (QuasiRegularValuation TropicalSemiringValue a ()), Ord a)
    => [DistanceMap a]
    -> Query a
    -> Process TropicalSemiringValue
answerQuery vs (source, target) = fmap head $ answerQueries vs [source] target

answerQueries :: (Show a, Typeable a, Binary a,  Binary (QuasiRegularValuation TropicalSemiringValue a ()), Ord a)
    => [DistanceMap a]
    -> [a]
    -> a
    -> Process [TropicalSemiringValue]
answerQueries vs sources target = do
    results <- answerQueriesM k $ map (\s -> S.fromList [s, target]) sources
    pure $ map (\(s, r) -> getDistance (solution r) (s, target)) $ zip sources results

    where
        k = knowledgeBase vs target

