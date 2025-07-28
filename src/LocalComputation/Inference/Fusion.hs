module LocalComputation.Inference.Fusion (
    fusion
) where
import qualified Data.Set                          as S
import           LocalComputation.ValuationAlgebra (Domain,
                                                    Valuation (eliminate, label),
                                                    combines1)
import           Numeric.Natural                   (Natural)

data WithId a = WithId {
      id      :: Natural
    , content :: a
}

instance Eq (WithId a) where
    x == y = x.id == y.id

instance Ord (WithId a) where
    x <= y = x.id <= y.id

data FusionError = XNotSubsetOfDPhi

-- TODO: Unsafe - doesn't verify query is subset of domain of combination of all valuations.
-- TODO: We can use elimination sequence here, but the fusion algorithm specifies that we
-- don't eliminate any variables in the query - so how is our 'elimination seq' impacted by
-- this? How do we still ensure we have a good elimination seq?
-- A: Use that one step lookahead from that local computation paper.
--    Wait; p371 of generic inference say something about the treewidth indicating an upper
--    bound on the time complexity of variable elimination?

-- | Performs fusion on the given set of valuations to calculate the combination of
-- all valuations projected to the given domain.
--
-- Based on the pseudocode of Algorithm 3.1 found in Marc Pouly's "Generic Inference".
fusion :: (Valuation v, Show a, Show b, Ord a, Ord b)
    => [v a b]
    -> Domain a
    -> Either FusionError (v a b)
fusion vs x
    | not $ S.isSubsetOf x dPhi = Left XNotSubsetOfDPhi
    | otherwise                 = Right $ fusion' nextId vsWithIds (S.toList (S.difference dPhi x))
    where
        dPhi = foldr (S.union . label) S.empty vs

        vsWithIds = S.fromList $ zipWith WithId [0..] vs
        nextId = fromIntegral $ length vs

fusion' :: (Valuation v, Show a, Show b, Ord a, Ord b) => Natural -> S.Set (WithId (v a b)) -> [a] -> v a b
fusion' _        upperPsi []     = combines1 $ map (.content) $ S.toList upperPsi
fusion' uniqueId upperPsi (y:ys) = fusion' (uniqueId + 1) upperPsi' ys
    where
        upperGamma = S.fromList [phi | phi <- S.toList upperPsi, y `S.member` label phi.content]
        psi = combines1 $ map (.content) $ S.toList upperGamma
        upperPsi' = S.union (S.difference upperPsi upperGamma)
                            (S.singleton (WithId uniqueId $ eliminate psi (S.singleton y)))
