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
    , unsafeQuery
    , queryDrawGraph
    , queries
    , unsafeQueries
    , queriesDrawGraph
    , queryDPDrawGraph
    , Mode (..)
    , Error (..)
) where
import           Control.Monad.IO.Class                         (MonadIO)
import           LocalComputation.Inference.MessagePassing      (SerializableValuation)
import qualified LocalComputation.Inference.ShenoyShafer        as SS (queries)
import           LocalComputation.LocalProcess                  (run)
import           LocalComputation.ValuationAlgebra

import qualified Data.Set                                       as S
import qualified LocalComputation.Inference.DynamicProgramming  as D
import qualified LocalComputation.Inference.Fusion              as F
import qualified LocalComputation.Inference.JoinTree.Diagram    as D
import qualified LocalComputation.LabelledMatrix                as M
import qualified LocalComputation.ValuationAlgebra.QuasiRegular as Q

data Error = QueryNotSubsetOfValuations deriving (NFData, Generic, Show)

data Mode = BruteForce | Fusion | Shenoy deriving (Show, Eq)

-- | Compute inference using the given mode to return valuations with the given domains.
queries :: (SerializableValuation v a, NFData (v a), MonadIO m, Show (v a))
    => Mode -> [v a] -> [Domain a] -> Either Error (m [v a])
queries = queriesDrawGraph D.def

queriesDrawGraph :: (SerializableValuation v a, Show (v a), NFData (v a), MonadIO m)
    => D.DrawSettings -> Mode -> [v a] -> [Domain a] -> Either Error (m [v a])
queriesDrawGraph _ _ vs qs
    | not $ queryIsCovered vs qs    = Left  $ QueryNotSubsetOfValuations
queriesDrawGraph _ BruteForce vs qs = Right $ pure $ bruteForces vs qs
queriesDrawGraph _ Fusion     vs qs = Right $ mapM (\q -> pure $ F.fusion vs q) qs
queriesDrawGraph s Shenoy     vs qs = Right $ run $ SS.queries s vs qs

queryIsCovered :: (Foldable t, ValuationFamily v, Var a)
    => [v a]
    -> t (S.Set a)
    -> Bool
queryIsCovered vs qs = not $ any (\q -> not $ S.isSubsetOf q coveredDomain) qs
    where
        coveredDomain = foldr S.union S.empty (map label vs)

-- | Perform a [query] using a [D]ynamic [P]rogramming implementation and draw a graph
-- of the join tree used during inference.
queryDPDrawGraph :: (MonadIO m, Q.QSemiringValue b, Show b, Var a, NFData a, NFData b, Serializable a, Serializable b)
    => D.DrawSettings
    -> [Q.Valuation b a]
    -> Domain a
    -> Either Error (m (M.LabelledMatrix a () b))
queryDPDrawGraph _ vs q
    | not $ queryIsCovered vs [q] = Left $ QueryNotSubsetOfValuations
queryDPDrawGraph s vs q           = Right $ run $ fmap D.solution $ F.fusionPass s vs q

--------------------------------------------------------------------------------
-- Singular variants
--------------------------------------------------------------------------------

-- | Compute inference using the given mode to return a valuation with the given domain.
query :: (SerializableValuation v a, NFData (v a), MonadIO m, Show (v a))
    => Mode -> [v a] -> Domain a -> Either Error (m (v a))
query mode vs q = fmap (fmap head) $ queries mode vs [q]

queryDrawGraph :: (SerializableValuation v a, Show (v a), NFData (v a), MonadIO m)
    => D.DrawSettings -> Mode -> [v a] -> Domain a -> Either Error (m (v a))
queryDrawGraph s mode vs q = fmap (fmap head) $ queriesDrawGraph s mode vs [q]

--------------------------------------------------------------------------------
-- Unsafe variants
--------------------------------------------------------------------------------

-- | Unsafe variant of `queries` - will throw if a query is not subset of the
-- domain the given valuations cover.
unsafeQueries :: (SerializableValuation v a, NFData (v a), MonadIO m, Show (v a))
    => Mode -> [v a] -> [Domain a] -> m [v a]
unsafeQueries mode vs qs = case queries mode vs qs of
                            Left e  -> error (show e)
                            Right r -> r


-- | Unsafe variant of `query` - will throw if query is not subset of the
-- domain the given valuations cover.
unsafeQuery :: (SerializableValuation v a, NFData (v a), MonadIO m, Show (v a))
 => Mode -> [v a] -> Domain a -> m (v a)
unsafeQuery mode vs q = fmap head $ unsafeQueries mode vs [q]

--------------------------------------------------------------------------------
-- Brute force (simpliest possible inference implementation)
--------------------------------------------------------------------------------

-- | Basic brute force computation, does not use local computation.
--
-- __Warning__: Assumes that the given queries are subsets of the covered domain - this should be checked
-- by the caller.
--
-- __Warning__: Assumes the given list of valuations is not empty.
bruteForces :: (ValuationFamily v, Var a)
    => [v a]
    -> [Domain a]
    -> [v a]
bruteForces vs qs = map (\q -> project combined q) qs
    where
        combined = combines1 vs
