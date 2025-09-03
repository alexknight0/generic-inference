{-# LANGUAGE ScopedTypeVariables #-}

module LocalComputation.Inference.Fusion (
    fusion
) where
import           Control.Distributed.Process               (expect)
import           Control.Monad                             (replicateM)
import qualified Data.Set                                  as S
import qualified LocalComputation.Inference.JoinTree       as JT
import qualified LocalComputation.Inference.MessagePassing as MP
import           LocalComputation.ValuationAlgebra         (Domain,
                                                            Valuation (eliminate, label),
                                                            Var, combines1,
                                                            project)
import           Numeric.Natural                           (Natural)

data WithId a = WithId {
      id      :: Natural
    , content :: a
}

instance Eq (WithId a) where
    x == y = x.id == y.id

instance Ord (WithId a) where
    x <= y = x.id <= y.id

-- TODO: We can use elimination sequence here, but the fusion algorithm specifies that we
-- don't eliminate any variables in the query - so how is our 'elimination seq' impacted by
-- this; but how does this affect *calculation* of the elimation seq?
-- i.e. Do we remove them before or after calculating the elimination seq?
-- A: Use that one step lookahead from that local computation paper.
--    Wait; p371 of generic inference say something about the treewidth indicating an upper
--    bound on the time complexity of variable elimination?

-- | Performs fusion on the given set of valuations to calculate the combination of
-- all valuations projected to the given domain.
--
-- Based on the pseudocode of Algorithm 3.1 found in Marc Pouly's "Generic Inference".
--
-- __Warning__: Assumes that the query is a subset of the covered domain - this should be checked
-- by the caller.
fusion :: (Valuation v, Var a)
    => [v a]
    -> Domain a
    -> v a
fusion vs x = fusion' nextId vsWithIds (S.toList dPhiMinusX)
    where
        dPhi = foldr S.union S.empty (map label vs)
        dPhiMinusX = S.difference dPhi x

        vsWithIds = S.fromList $ zipWith WithId [0..] vs
        nextId = fromIntegral $ length vs

fusion' :: (Valuation v, Var a) => Natural -> S.Set (WithId (v a)) -> [a] -> v a
fusion' _        upperPsi []     = combines1 $ map (.content) $ S.toList upperPsi
fusion' uniqueId upperPsi (y:ys) = fusion' (uniqueId + 1) upperPsi' ys
    where
        upperGamma = S.filter (\phi -> S.member y (label phi.content)) upperPsi
        psi = combines1 $ map (.content) $ S.toList upperGamma
        upperPsi' = S.union (S.difference upperPsi upperGamma)
                            (S.singleton (WithId uniqueId $ eliminate psi (S.singleton y)))


--------------------------------------------------------------------------------
-- Fusion as a message passing scheme
--------------------------------------------------------------------------------
-- TODO: Our current join tree construction method does not set the root node as we desire.
-- Wait i think we can use the join tree algorithm we just have to make sure the query doesn't get eliminated
-- but everything else does?

-- Current idea: fusion will expose a function that gives u 'inferreddata' for fusion. This
-- will then be called by solution construction - then solution construction is the one that
-- will have it's own implementation.

fusionWithMessagePassing :: (Valuation v, Var a)
    => [v a] -> Domain a -> v a
fusionWithMessagePassing = undefined

nodeActions :: (MP.SerializableValuation v a) => MP.NodeActions v a
nodeActions this neighbours resultPort = do

    -- Uses the property that the root node is the only node whose label is larger
    -- than all its neighbours
    -- case isRootNode this of
    --     False -> MP.collect this neighbours



    -- ... because the root node never sends out a message, messages will naturally propagate
    -- down to the root node as each node will always send its message to the neighbour that
    -- didn't send it a message.

    -- MP.collect this neighbours






    undefined

    where

isRootNode :: MP.NodeWithProcessId a -> [MP.NodeWithProcessId a] -> Bool
isRootNode node neighbours = all (\n -> node.id > n.id) neighbours


-- | Computes a message to send to the given neighbour.
--
-- Computing this message consists of:
--  1. combining all messages in the sender's postbox that don't come from the neighbour
--  2. combining this result with the sender's valuation
--  3. projecting the result to the intersection of the sender and neighbour's domain
-- computeMessage :: forall v a . (Valuation v, Var a)
--     => [MP.Message (v a)]
--     -> MP.NodeWithProcessId (v a)
--     -> MP.NodeWithProcessId (v a)
--     -> v a
-- computeMessage postbox sender recipient = eliminate combines1 (sender.node.v : map (.msg) postbox)
--     where
--         test :: v a
--         test = sender.node.v




