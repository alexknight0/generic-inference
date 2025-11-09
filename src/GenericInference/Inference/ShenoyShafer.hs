{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GenericInference.Inference.ShenoyShafer (
      queries
) where

import           Control.Distributed.Process                           hiding
                                                                       (Message)

import           Data.Set                                              (intersection)


import           Control.Monad.IO.Class                                (MonadIO)
import qualified Data.List                                             as L
import           Data.Maybe                                            (fromJust)
import           GenericInference.Inference.JoinTree                   (Node (..),
                                                                        binaryJoinForest)
import qualified GenericInference.Inference.JoinTree                   as J
import qualified GenericInference.Inference.JoinTree                   as JF
import qualified GenericInference.Inference.JoinTree.Diagram           as D
import qualified GenericInference.Inference.JoinTree.Forest            as JF
import qualified GenericInference.Inference.JoinTree.Tree              as JT
import qualified GenericInference.Inference.MessagePassing             as MP
import qualified GenericInference.Inference.MessagePassing.Distributed as DMP
import qualified GenericInference.Inference.MessagePassing.Threads     as TMP
import qualified GenericInference.Inference.Statistics                 as S
import           GenericInference.LocalProcess                         (run)
import           GenericInference.ValuationAlgebra

type ResultingTree v a = JF.JoinForest v a

-- | Extracts a given query from the query results.
--
-- Assumes query is subset of the domain the given valuations cover.
extractQueryResults :: forall v a. (Var a)
    => [Domain a]
    -> ResultingTree v a
    -> [v a]
extractQueryResults queryDomains results = map f queryDomains
    where
        -- Find valuation of a query node with domain equal to query then get the valuation
        -- Needs to be a query node as `calculate` of `MessagePassing.Threads` only calculates
        -- results on query nodes (although this can be changed)
        f :: Domain a -> v a
        f q = (.v) $ fromJust $ L.find (\n -> n.t == JF.Query && q == n.d) (JF.vertexList results)


-- | Performs shenoy shafer inference.
--
-- Assumes query is subset of the domain the given valuations cover.
queries :: (NFData (v a), DMP.SerializableValuation v a, Show (v a), MonadIO m, NFData a)
    => MP.Mode
    -> D.DrawSettings
    -> [v a]
    -> [Domain a]
    -> m (S.WithStats [v a])
queries mode settings vs queryDomains = do
    drawForest settings.beforeInference forestBeforeInference

    forestAfterInference <- case mode of
                            MP.Distributed -> run $ DMP.messagePassing forestBeforeInference (nodeActions updateTree)
                            MP.Threads     -> pure $ TMP.messagePassing updateTree forestBeforeInference

    drawForest settings.afterInference forestAfterInference

    pure $ S.withStats stats $ extractQueryResults queryDomains forestAfterInference

    where
        forestBeforeInference = binaryJoinForest vs queryDomains
        stats = S.fromForest forestBeforeInference

        drawForest Nothing         _    = pure ()
        drawForest (Just filename) tree = liftIO $ D.drawForest filename tree

        -- | If we are drawing the tree after inference, we should update the tree
        -- with the inference results so we can draw it properly.
        updateTree = case settings.afterInference of
                                Nothing -> False
                                Just _  -> True



-- | `updateTree` is a boolean representing whether non-query nodes should have their value
-- updated in the tree. Useful if the tree is going be utilised after inference such as for
-- drawing in a graph.
nodeActions :: (DMP.SerializableValuation v a)
    => Bool
    -> DMP.NodeActions v a
nodeActions updateTree this neighbours resultPort = do

    postbox <- case neighbours of
                    [] -> pure []
                    _  -> do
                        -- Collect and distribute assume the given node has a neighbour
                        collectResults    <- DMP.collect                   this neighbours computeMessage'
                        distributeResults <- DMP.distribute collectResults this neighbours computeMessage
                        pure $ distributeResults.postbox

    case updateTree || JT.isQueryNode this.node of
        True -> do
            -- Always send result back to parent process if query node
            let result = combines1 (this.node.v : map (.msg) postbox)
            sendChan resultPort $ J.changeContent this.node result

        False -> pure ()  -- Don't need to bother updating valuations for non-query nodes
                          -- unless requested by `updateTree`.

-- | Computes a message to send to the given neighbour.
--
-- Computing this message consists of:
--  1. combining all messages in the sender's postbox that don't come from the neighbour
--  2. combining this result with the sender's valuation
--  3. projecting the result to the intersection of the sender and neighbour's domain
computeMessage :: (ValuationFamily v, Var a)
    => [DMP.Message (v a)]
    -> DMP.NodeWithPid v a
    -> DMP.NodeWithPid v a
    -> v a
computeMessage postbox sender recipient = computeMessage' (filter (\msg -> msg.sender /= recipient.id) postbox)
                                                          sender
                                                          recipient

-- | Same as `computeMessage` except doesn't filter the given postbox for messages that don't come from the
-- recipient. Hence should only be used when it is known that none of the messages in the postbox come
-- from the recipient.
computeMessage' :: (ValuationFamily v, Var a)
    => [DMP.Message (v a)]
    -> DMP.NodeWithPid v a
    -> DMP.NodeWithPid v a
    -> v a
computeMessage' postbox sender recipient = project (combines1 (sender.node.v : map (.msg) postbox))
                                                   (intersection sender.node.d recipient.node.d)

