{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

{- | Exposes functions for computing inference. -}
module LocalComputation.Inference (
      query
    , query'
    , Mode (..)
    , Error (..)
) where
import           Control.Monad.IO.Class                  (MonadIO)
import qualified LocalComputation.Inference.ShenoyShafer as SS (answerQueryM)
import           LocalComputation.LocalProcess           (run)
import           LocalComputation.ValuationAlgebra       (Domain, combines1,
                                                          label, project)

import           Control.DeepSeq                         (NFData)
import           Data.Binary                             (Binary)
import           GHC.Generics                            (Generic)
import           Type.Reflection                         (Typeable)

import qualified Data.Set                                as S
import qualified LocalComputation.Inference.Fusion       as F
import           LocalComputation.ValuationAlgebra       (Valuation)

data Error = QueryNotSubsetOfValuations deriving (NFData, Generic, Show)

data Mode = Baseline | Fusion | Shenoy

-- | Compute inference using the given mode to return a valuation with the given domain.
query :: (
      Valuation v
    , NFData (v a b)
    , Ord a, Ord b
    , Binary a, Binary (v a b)
    , Typeable v, Typeable a, Typeable b
    , Show a, Show b
    , MonadIO m
    )
 => Mode -> [v a b] -> Domain a -> Either Error (m (v a b))
query mode vs q
    | not $ S.isSubsetOf q coveredDomain = Left QueryNotSubsetOfValuations
    | otherwise                          = Right $ query'' mode vs q
    where
        coveredDomain = foldr S.union S.empty (map label vs)

-- | Unsafe variant of `query` - will throw if query is not subset of the
-- domain the given valuations cover.
query' :: (
      Valuation v
    , NFData (v a b)
    , Ord a, Ord b
    , Binary a, Binary (v a b)
    , Typeable v, Typeable a, Typeable b
    , Show a, Show b
    , MonadIO m
    )
 => Mode -> [v a b] -> Domain a -> m (v a b)
query' mode vs d = case query mode vs d of
                        Left e       -> error (show e)
                        Right result -> result

-- | Unsafe variant of `query'` - undefined behaviour if query is not a subset
-- of the domain the given valuations cover.
query'' :: (
      Valuation v
    , NFData (v a b)
    , Ord a, Ord b
    , Binary a, Binary (v a b)
    , Typeable v, Typeable a, Typeable b
    , Show a, Show b
    , MonadIO m
    )
 => Mode -> [v a b] -> Domain a -> m (v a b)
query'' Baseline = (pure .) . baseline
query'' Fusion   = (pure .) . F.fusion
query'' Shenoy   = (run .)  . SS.answerQueryM

-- | Basic brute force computation, does not use local computation.
--
-- __Warning__: Assumes that the query is a subset of the covered domain - this should be checked
-- by the caller.
baseline :: (Valuation v, Show a, Show b, Ord a, Ord b)
    => [v a b]
    -> Domain a
    -> (v a b)
baseline vs q = project (combines1 vs) q

-- TODO: The above function is untested. The below function seemed to be the one that passed tests,
-- but it seems wrong?
--
-- baseline vs x = project (combines1 vs) dPhi
--     where
--         dPhi = foldr (S.union . label) S.empty vs
--

