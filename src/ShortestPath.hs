{-# LANGUAGE FlexibleContexts #-}
module ShortestPath
    ( answerQuery
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

knowledgeBase :: (Ord a, Show a) => [DistanceMap a] -> Query a -> Knowledgebase a
knowledgeBase xs q = map f xs
    where
        f m = fromJust $ create m' b
            where
                m' = makeMapSquare m
                b = M.fromList [((a, ()), if a == snd q then one else zero) | a <- S.toList $ fst (M.domain m')]

-- knowledgeBase2 :: (Ord a, Show a) => [DistanceMap a] -> [Query a] -> Knowledgebase a
-- knowledgeBase2 xs q = map f xs
--     where
--         f m = fromJust $ create m' b
--             where
--                 m' = makeMapSquare m
--                 b = M.fromList [((a, ()), if a `elem` snd q then one else zero) | a <- S.toList $ fst (M.domain m')]

-- | Retuns an entry from a distance map. Unsafe.
getDistance :: (Ord a, Show a) => DistanceVector a -> Query a -> TropicalSemiringValue
getDistance m (source, _) = fromJust $ M.find (source, ()) m

answerQuery :: (Show a, Typeable a, Binary a,  Binary (QuasiRegularValuation TropicalSemiringValue a ()), Ord a)
    => [DistanceMap a]
    -> Query a
    -> Process TropicalSemiringValue
answerQuery xs q = do
    m <- answerQueryM k (S.fromList [fst q, snd q])
    pure $ getDistance (solution m) q

    where
        k = knowledgeBase xs q

-- answerQueries :: (Show a, Typeable a, Binary a,  Binary (QuasiRegularValuation TropicalSemiringValue a ()), Ord a)
--     => [DistanceMap a]
--     -> [Query a]
--     -> Process [TropicalSemiringValue]
-- answerQueries xs qs = do
--     m <- answerQueriesM k $ map (\(qSource, qDestination) -> S.fromList [qSource, qDestination]) qs
--     pure $ map (getDistance (solution m)) qs
--
--     where
--         k = knowledgeBase xs q




