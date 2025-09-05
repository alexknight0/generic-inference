{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

{-
Functions performing inference may impose many constraints on the caller
that are not actually necessary for the mode selected. For example, it is not
necessary that the valuations provided are 'Serializable' for the caller to use
the 'BruteForce' mode. This is done for simplicity, but could change if undesired.
-}

{- | Exposes functions for computing inference. -}
module LocalComputation.Inference (
      query
    , query'
    , queryDrawGraph
    , queries
    , queries'
    , queriesDrawGraph
    , Mode (..)
    , Error (..)
) where
import           Control.Monad.IO.Class                      (MonadIO)
import qualified LocalComputation.Inference.ShenoyShafer     as SS (queries)
import           LocalComputation.LocalProcess               (run)
import           LocalComputation.ValuationAlgebra           (Domain, Valuation,
                                                              Var, combines1,
                                                              label, project)

import           Control.DeepSeq                             (NFData)
import           GHC.Generics                                (Generic)
import           LocalComputation.Inference.MessagePassing   (SerializableValuation)

import qualified Data.Set                                    as S
import qualified LocalComputation.Inference.Fusion           as F
import qualified LocalComputation.Inference.JoinTree.Diagram as D

data Error = QueryNotSubsetOfValuations deriving (NFData, Generic, Show)

data Mode = BruteForce | Fusion | Shenoy deriving (Show)

-- | Compute inference using the given mode to return valuations with the given domains.
queries :: (SerializableValuation v a, NFData (v a), MonadIO m, Show (v a))
    => Mode -> [v a] -> [Domain a] -> Either Error (m [v a])
queries _ vs qs
    | not $ queryIsCovered vs qs = Left  $ QueryNotSubsetOfValuations
queries BruteForce vs qs         = Right $ pure $ baselines vs qs
queries Fusion     vs qs         = Right $ mapM (\q -> pure $ F.fusion vs q) qs
queries Shenoy     vs qs         = Right $ run $ SS.queries D.def vs qs

queryIsCovered :: (Foldable t, Valuation v, Var a)
    => [v a]
    -> t (S.Set a)
    -> Bool
queryIsCovered vs qs = not $ any (\q -> not $ S.isSubsetOf q coveredDomain) qs
    where
        coveredDomain = foldr S.union S.empty (map label vs)

--------------------------------------------------------------------------------
-- Singular variants
--------------------------------------------------------------------------------




-- | Unsafe variant of `queries` - will throw if a query is not subset of the
-- domain the given valuations cover.
queries' :: (SerializableValuation v a, NFData (v a), MonadIO m, Show (v a))
    => Mode -> [v a] -> [Domain a] -> m [v a]
queries' mode vs qs = case queries mode vs qs of
                            Left e  -> error (show e)
                            Right r -> r

queriesDrawGraph :: (SerializableValuation v a, Show (v a), NFData (v a), MonadIO m)
    => D.DrawSettings -> Mode -> [v a] -> [Domain a] -> Either Error (m [v a])
queriesDrawGraph _ _ vs qs
    | not $ queryIsCovered vs qs = Left $ QueryNotSubsetOfValuations
queriesDrawGraph s Shenoy vs qs = Right $ run $ SS.queries s vs qs
queriesDrawGraph _    _   _  _  = error "Not implemented"

queryDrawGraph :: (SerializableValuation v a, Show (v a), NFData (v a), MonadIO m)
    => D.DrawSettings -> Mode -> [v a] -> Domain a -> Either Error (m (v a))
queryDrawGraph s mode vs q = fmap (fmap head) $ queriesDrawGraph s mode vs [q]

-- | Compute inference using the given mode to return a valuation with the given domain.
query :: (SerializableValuation v a, NFData (v a), MonadIO m, Show (v a))
    => Mode -> [v a] -> Domain a -> Either Error (m (v a))
query mode vs q = fmap (fmap head) $ queries mode vs [q]

-- | Unsafe variant of `query` - will throw if query is not subset of the
-- domain the given valuations cover.
query' :: (SerializableValuation v a, NFData (v a), MonadIO m, Show (v a))
 => Mode -> [v a] -> Domain a -> m (v a)
query' mode vs q = fmap head $ queries' mode vs [q]

--------------------------------------------------------------------------------
-- Baseline (brute force implementation)
--------------------------------------------------------------------------------

-- | Basic brute force computation, does not use local computation.
--
-- __Warning__: Assumes that the query is a subset of the covered domain - this should be checked
-- by the caller.
--
-- __Warning__: Assumes the given list of valuations is not empty.
baseline :: (Valuation v, Var a)
    => [v a]
    -> Domain a
    -> v a
baseline vs q = head $ baselines vs [q]

-- | Basic brute force computation, does not use local computation.
--
-- __Warning__: Assumes that the given queries are subsets of the covered domain - this should be checked
-- by the caller.
--
-- __Warning__: Assumes the given list of valuations is not empty.
baselines :: (Valuation v, Var a)
    => [v a]
    -> [Domain a]
    -> [v a]
baselines vs qs = map (\q -> project combined q) qs
    where
        combined = combines1 vs
