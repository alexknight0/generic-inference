{-# LANGUAGE ScopedTypeVariables #-}

module LocalComputation.Inference.Fusion (
      fusion
    , fusionWithMessagePassing
    , fusionWithMessagePassingDraw
) where
import qualified Algebra.Graph                               as DG
import           Control.Distributed.Process                 (Process, expect,
                                                              liftIO, sendChan)
import           Control.Exception                           (assert)
import           Control.Monad                               (replicateM, void)
import qualified Data.Set                                    as S
import qualified Data.Text.Lazy                              as LT
import           Debug.Trace                                 (trace, traceId,
                                                              traceShow,
                                                              traceShowId)
import qualified LocalComputation.Inference.JoinTree         as JT
import qualified LocalComputation.Inference.JoinTree.Diagram as D
import qualified LocalComputation.Inference.MessagePassing   as MP
import           LocalComputation.ValuationAlgebra           (Domain,
                                                              Valuation (eliminate, label),
                                                              Var, combines1,
                                                              project)
import           Numeric.Natural                             (Natural)
import           Text.Pretty.Simple                          (pShow)

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
-- TODO: I think for fusion we want a new join tree construction algorithm that will help us ensure
-- only one variable is eliminated at a time. We accept the fact that fusion with a query equal
-- to the full set of variables is equal to brute force. We could even check this fact with Naso.

-- TODO: Add:
-- __Warning__: Doesn't work on problems that have disconnected join trees

fusionWithMessagePassing :: (MP.SerializableValuation v a, Show (v a))
    => [v a] -> Domain a -> Process (DG.Graph (JT.Node (v a)))
fusionWithMessagePassing vs queryDomain = MP.messagePassing (JT.redirectToQueryNode queryDomain $ JT.baseJoinTree vs [queryDomain]) nodeActions

fusionWithMessagePassingDraw :: (MP.SerializableValuation v a, Show (v a))
    => FilePath -> [v a] -> Domain a -> Process (DG.Graph (JT.Node (v a)))
fusionWithMessagePassingDraw filename vs queryDomain = do
    results <- MP.messagePassing (JT.redirectToQueryNode queryDomain $ JT.baseJoinTree vs [queryDomain]) nodeActions
    liftIO $ D.draw filename results
    pure results


nodeActions :: (MP.SerializableValuation v a) => MP.NodeActions v a
nodeActions this neighbours resultPort = do

    postbox <- case isRootNode this neighbours of
        -- If root node collect a message from each neighbour, but don't send a message.
        -- If the root node never sends out a message, messages will naturally propagate
        -- down to the root node following the logic that each node sends its message
        -- to the neighbour that doesn't send it a message.
        True  -> replicateM (length neighbours) expect

        -- If not root node, execute collect algorithm.
        False -> fmap (.postbox) $ MP.collect this neighbours computeMessage

    -- TODO: In the non-root-node case, we duplicated a 'combines' operation here.
    let result = combines1 (this.node.v : map (.msg) postbox)
    -- trace (foo result) (pure ())
    assert (this.node.d == label result) (pure ())
    sendChan resultPort $ JT.changeContent this.node result

    where
        -- foo x
        --     | this.node.d /= label x = LT.unpack $ pShow (this.node.d, label x)
        --     | otherwise = ""

-- Uses the property that the root node is the only node whose label is larger
-- than all its neighbours
-- TODO: Fix
isRootNode :: MP.NodeWithPid a -> [MP.NodeWithPid a] -> Bool
isRootNode node neighbours = node.node.t == JT.Query -- all (\n -> node.id > n.id) neighbours


-- TODO: Update doc.

-- | Computes a message to send to the given neighbour.
--
-- Computing this message consists of:
--  1. combining all messages in the sender's postbox that don't come from the neighbour
--  2. combining this result with the sender's valuation
--  3. projecting the result to the intersection of the sender and neighbour's domain
computeMessage :: forall v a . (Valuation v, Var a)
    => [MP.Message (v a)]
    -> MP.NodeWithPid (v a)
    -> MP.NodeWithPid (v a)
    -> v a
computeMessage postbox sender recipient
-- TODO: fix
    | recipient.node.t `elem` [JT.Projection, JT.Query] = eliminate combined varsToEliminate
    | otherwise                         = combined
    where
        varsToEliminate = S.difference sender.node.d recipient.node.d

        combined = combines1 (sender.node.v : map (.msg) postbox)





