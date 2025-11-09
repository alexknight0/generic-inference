{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}

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
    , unsafeQueryDrawGraph
    , queriesWithStats
    , queryWithStats
    , solution
    , solutionWithStats
    , Mode (..)
    , Error (..)
    , S.WithStats (..)
) where
import           Control.Monad.IO.Class                                (MonadIO)
import qualified LocalComputation.Inference.MessagePassing.Distributed as D (SerializableValuation)
import qualified LocalComputation.Inference.ShenoyShafer               as SS (queries)
import           LocalComputation.ValuationAlgebra

import qualified Data.Set                                              as S
import qualified LocalComputation.Inference.DynamicProgramming         as D
import qualified LocalComputation.Inference.Fusion                     as F
import qualified LocalComputation.Inference.JoinTree.Diagram           as D
import qualified LocalComputation.Inference.MessagePassing             as MP
import qualified LocalComputation.Inference.Statistics                 as S hiding
                                                                            (empty)
import           LocalComputation.Utils.Composition

-- | Errors that could occur during inference
--
-- UnanswerableQuery indicates that there exists a variable in the query that is not
-- present in any valuation, making the query impossible to answer.
--
-- The problem with an empty query is that it is always trivially covered by the knowledgebase
-- of zero valuations. As a result, the empty query needs to have a set, hardcoded answer.
--
-- Developers note: if we impose that we must have at least one valuation, we still have
-- another problem to handle before empty queries will work. The binary join tree construction
-- method will place the query node in a disconnected join tree as it does not relate to any
-- other node. The valuation attached to this query node will then never be updated from 'identity'
-- as it never interacts with a non-identity node. A different node that has an empty domain but
-- is attached to a join tree with valuations should be selected and labelled the query node.
data Error = UnanswerableQuery | EmptyQuery deriving (NFData, Generic, Show)

data Mode = BruteForce | Fusion | Shenoy { _mode :: MP.Mode } deriving (Show, Eq)

type ConstrainedValuation v a = (
                                -- Required for display of join trees
                                  Show (v a)
                                -- Required for use of 'distributed' mode
                                , D.SerializableValuation v a
                                -- Required for parallelized ('distributed' and 'threads' modes)
                                , NFData (v a), NFData a
                                )

-- | Compute inference using the given mode to return valuations with the given domains.
queries :: (ConstrainedValuation v a, MonadIO m)
    => Mode -> [v a] -> [Domain a] -> Either Error (m [v a])
queries = queriesDrawGraph D.def

queriesDrawGraph :: (ConstrainedValuation v a, MonadIO m)
    => D.DrawSettings -> Mode -> [v a] -> [Domain a] -> Either Error (m [v a])
queriesDrawGraph = fmap (fmap (.c)) .:: queriesWithStats

-- | Execute a series of queries, and return the results with some statistics. For the returned
-- statistics to be meaningful, statistic collection must be enabled by passing "-DCOUNT_OPERATIONS=1".
queriesWithStats :: (ConstrainedValuation v a, MonadIO m)
    => D.DrawSettings -> Mode -> [v a] -> [Domain a] -> Either Error (m (S.WithStats [v a]))
queriesWithStats _ _ vs qs
    | not $ queryIsCovered vs qs    = Left  $ UnanswerableQuery
    | any S.null qs                 = Left  $ EmptyQuery
queriesWithStats _ BruteForce vs qs = Right $ pure $ bruteForces vs qs
queriesWithStats _ Fusion     vs qs = Right $ pure $ S.lift $ map (\q -> F.fusion vs q) qs
queriesWithStats s (Shenoy m) vs qs = Right $ SS.queries m s vs qs

queryIsCovered :: (Foldable t, Valuation v a)
    => [v a]
    -> t (S.Set a)
    -> Bool
queryIsCovered vs qs = not $ any (\q -> not $ S.isSubsetOf q coveredDomain) qs
    where
        coveredDomain = foldr S.union S.empty (map label vs)

solution :: (ConstrainedValuation v a, MonadIO m)
    => MP.Mode
    -> [v a]
    -> Domain a
    -> Either Error (m (VarAssignment v a))
solution = fmap (fmap (.c)) .:. solutionWithStats D.def

solutionWithStats :: (ConstrainedValuation v a, MonadIO m)
    => D.DrawSettings
    -> MP.Mode
    -> [v a]
    -> Domain a
    -> Either Error (m (S.WithStats (VarAssignment v a)))
solutionWithStats _ _ vs q
    | not $ queryIsCovered vs [q] = Left $ UnanswerableQuery
solutionWithStats s mode vs q = Right . fmap (fmap D.solution) $ F.fusionPass s mode vs q

--------------------------------------------------------------------------------
-- Singular variants
--------------------------------------------------------------------------------

-- | Compute inference using the given mode to return a valuation with the given domain.
query :: (ConstrainedValuation v a, MonadIO m)
    => Mode -> [v a] -> Domain a -> Either Error (m (v a))
query = queryDrawGraph D.def

queryDrawGraph :: (ConstrainedValuation v a, MonadIO m)
    => D.DrawSettings -> Mode -> [v a] -> Domain a -> Either Error (m (v a))
queryDrawGraph = fmap (fmap (.c)) .:: queryWithStats

queryWithStats :: (ConstrainedValuation v a, MonadIO m)
    => D.DrawSettings -> Mode -> [v a] -> Domain a -> Either Error (m (S.WithStats (v a)))
queryWithStats s mode vs q = fmap (fmap (fmap head)) $ queriesWithStats s mode vs [q]

--------------------------------------------------------------------------------
-- Unsafe variants
--------------------------------------------------------------------------------
fromErrorable :: Either Error a -> a
fromErrorable (Left e)  = error (show e)
fromErrorable (Right x) = x

-- | Unsafe variant of `queries` - will throw if a query is not subset of the
-- domain the given valuations cover.
unsafeQueries :: (ConstrainedValuation v a, MonadIO m)
    => Mode -> [v a] -> [Domain a] -> m [v a]
unsafeQueries = fromErrorable .:. queries

-- | Unsafe variant of `query` - will throw if query is not subset of the
-- domain the given valuations cover.
unsafeQuery :: (ConstrainedValuation v a, MonadIO m)
 => Mode -> [v a] -> Domain a -> m (v a)
unsafeQuery = fromErrorable .:. query

-- | Unsafe variant of `queryDrawGraph` - will throw if query is not subset of the
-- domain the given valuations cover.
unsafeQueryDrawGraph :: (ConstrainedValuation v a, MonadIO m)
    => D.DrawSettings -> Mode -> [v a] -> Domain a -> m (v a)
unsafeQueryDrawGraph = fromErrorable .:: queryDrawGraph

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
    -> S.WithStats [v a]
bruteForces vs qs = S.withStats stats $ map (\q -> project combined q) qs
    where
        combined = combines1 vs
        stats = S.fromLargestNode (Just combined) vs
