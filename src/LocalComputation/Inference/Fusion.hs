{-# LANGUAGE ScopedTypeVariables #-}

module LocalComputation.Inference.Fusion (
      fusion
    , fusionPass
) where
import           Control.Distributed.Process                 (Process, expect,
                                                              liftIO, sendChan)
import           Control.Exception                           (assert)
import           Control.Monad                               (replicateM)
import qualified Data.Set                                    as S
import qualified LocalComputation.Inference.JoinTree         as JT
import qualified LocalComputation.Inference.JoinTree.Diagram as D
import qualified LocalComputation.Inference.MessagePassing   as MP
import           LocalComputation.ValuationAlgebra           (Domain,
                                                              Valuation (eliminate, label),
                                                              Var, combines1)
import           Numeric.Natural                             (Natural)

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
-- Fusion applied to a join tree (used for dynamic programming)
--------------------------------------------------------------------------------
-- TODO: I think for fusion we want a new join tree construction algorithm that will help us ensure
-- only one variable is eliminated at a time. We accept the fact that fusion with a query equal
-- to the full set of variables is equal to brute force. We could even check this fact with Naso.

-- TODO: Can we make this work for disconnected join trees?

-- | Takes a join tree and returns the join tree after a fusion pass over a given join tree.
--
-- __Warning__: will fail if a disconnected join tree is given.
fusionPass :: (MP.SerializableValuation v a, Show (v a))
    => D.DrawSettings -> [v a] -> Domain a -> Process (JT.JoinTree (v a))
fusionPass settings vs queryDomain = do
    drawTree settings.beforeInference treeBeforeInference

    treeAfterInference <- MP.messagePassing' treeBeforeInference nodeActions

    drawTree settings.afterInference treeAfterInference

    pure treeAfterInference

    where
        treeBeforeInference = JT.collectTree vs queryDomain

        drawTree Nothing         _    = pure ()
        drawTree (Just filename) tree = liftIO $ D.drawTree filename tree

nodeActions :: (MP.SerializableValuation v a) => MP.NodeActions v a
nodeActions this neighbours resultPort = do

    postbox <- case isRootNode of
        -- If root node collect a message from each neighbour, but don't send a message.
        -- If the root node never sends out a message, messages will naturally propagate
        -- down to the root node following the logic that each node sends its message
        -- to the neighbour that doesn't send it a message.
        True  -> replicateM (length neighbours) expect

        -- If not root node, execute collect algorithm.
        False -> fmap (.postbox) $ MP.collect this neighbours computeMessage

    -- TODO: In the non-root-node case (most cases!), we duplicated a 'combines' operation here.
    let result = combines1 (this.node.v : map (.msg) postbox)
    assert (this.node.d == label result) (pure ())

    sendChan resultPort $ JT.changeContent this.node result

    where
        isRootNode = this.node.t == JT.Query

-- | Computes a message to send to the given neighbour.
--
-- Computing this message consists of:
--  1. combining all messages in the sender's postbox that don't come from the neighbour
--  2. combining this result with the sender's valuation
--  3. eliminating all variables not in the receivers domain
computeMessage :: forall v a . (Valuation v, Var a)
    => [MP.Message (v a)]
    -> MP.NodeWithPid (v a)
    -> MP.NodeWithPid (v a)
    -> v a
computeMessage postbox sender recipient = eliminate combined varsToEliminate
    where
        varsToEliminate = S.difference sender.node.d recipient.node.d

        combined = combines1 (sender.node.v : map (.msg) postbox)





