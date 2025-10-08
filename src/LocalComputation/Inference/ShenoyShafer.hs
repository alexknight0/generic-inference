{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LocalComputation.Inference.ShenoyShafer (
      queries
    , InferredData
) where

import           Control.Distributed.Process                           hiding
                                                                       (Message)

import           Data.Set                                              (intersection)


import           Control.DeepSeq                                       (rnf)
import           Control.Exception                                     (assert)
import           Control.Exception.Base                                (evaluate)
import qualified Data.List                                             as L
import           Data.Maybe                                            (fromJust)
import           LocalComputation.Inference.JoinTree                   (Node (..),
                                                                        baseJoinForest,
                                                                        binaryJoinForest)
import qualified LocalComputation.Inference.JoinTree                   as J
import qualified LocalComputation.Inference.JoinTree                   as JF
import qualified LocalComputation.Inference.JoinTree.Diagram           as D
import qualified LocalComputation.Inference.JoinTree.Forest            as JF
import qualified LocalComputation.Inference.JoinTree.Tree              as JT
import qualified LocalComputation.Inference.MessagePassing             as MP
import qualified LocalComputation.Inference.MessagePassing.Distributed as DMP
import qualified LocalComputation.Inference.MessagePassing.Threads     as TMP
import           LocalComputation.ValuationAlgebra
import           System.IO                                             (hFlush,
                                                                        stdout)

-- TODO: [Hypothesis]... Due to the high serialization cost, using the Cloud Haskell library to represent
-- the message passing process by treating each node as a seperate computer is not efficent.
-- However as Cloud Haskell allows easy implementation of cross-machine message passing, there
-- could still be value obtained from this approach if there does not exist a single
-- computer with enough cores to provide the performance required to compute a certain result.
-- In this case, however, ideally we would still not directly use Cloud Haskell to represent the
-- message passing process - it would be much more efficent to minimize serialization and transportation
-- costs by assigning groups of nodes who are 'close' to each other to one processor. That one processor
-- could even then use a multi-threaded approach to avoid serialization costs entirely. This would require
-- the subproblem of finding 'groups' of nodes in the larger graph.

-- TODO: Rename ResultingTree; stop exporting
type InferredData v a = JF.JoinForest (v a)

-- | Extracts a given query from the query results.
--
-- Assumes query is subset of the domain the given valuations cover.
extractQueryResult :: forall v a. (ValuationFamily v, Var a)
    => [Domain a]
    -> InferredData v a
    -> [v a]
extractQueryResult queryDomains results = map f queryDomains
    where
        -- Find valuation of a query node with domain equal to query then get the valuation
        -- Needs to be a query node as `calculate` of `MessagePassing.Threads` only calculates
        -- results on query nodes (although this can be changed)
        f :: Domain a -> v a
        f q = (.v) $ fromJust $ L.find (\n -> n.t == JF.Query && q == n.d) (JF.vertexList results)


-- | Performs shenoy shafer inference.
--
-- Assumes query is subset of the domain the given valuations cover.
queries :: (NFData (v a), DMP.SerializableValuation v a, Show (v a))
    => MP.Mode
    -> D.DrawSettings
    -> [v a]
    -> [Domain a]
    -> Process [v a]
queries mode settings vs queryDomains = do
    drawForest settings.beforeInference forestBeforeInference

    forestAfterInference <- case mode of
                            MP.Distributed ->        DMP.messagePassing forestBeforeInference nodeActions
                            MP.Threads     -> pure $ TMP.messagePassing forestBeforeInference

    drawForest settings.afterInference forestAfterInference

    pure $ extractQueryResult queryDomains forestAfterInference

    where
        forestBeforeInference = baseJoinForest vs queryDomains

        drawForest Nothing         _    = pure ()
        drawForest (Just filename) tree = liftIO $ D.drawForest filename tree

nodeActions :: (DMP.SerializableValuation v a)
    => DMP.NodeActions v a
nodeActions this neighbours resultPort = do

    postbox <- case neighbours of
                    [] -> pure []
                    _  -> do
                        -- Collect and distribute assume the given node has a neighbour
                        collectResults    <- DMP.collect                   this neighbours computeMessage'
                        distributeResults <- DMP.distribute collectResults this neighbours computeMessage
                        pure $ distributeResults.postbox

    case JT.isQueryNode this.node of
        True -> do
            -- Send result back to parent process
            let result = combines1 (this.node.v : map (.msg) postbox)
            assertLabelUnchanged result
            sendChan resultPort $ J.changeContent this.node result

        False -> pure ()  -- Don't need to bother updating valuations for non-query nodes.

    where
        assertLabelUnchanged result = assert (this.node.d == label result) $ pure ()

-- | Computes a message to send to the given neighbour.
--
-- Computing this message consists of:
--  1. combining all messages in the sender's postbox that don't come from the neighbour
--  2. combining this result with the sender's valuation
--  3. projecting the result to the intersection of the sender and neighbour's domain
computeMessage :: (ValuationFamily v, Var a)
    => [DMP.Message (v a)]
    -> DMP.NodeWithPid (v a)
    -> DMP.NodeWithPid (v a)
    -> v a
computeMessage postbox sender recipient = computeMessage' (filter (\msg -> msg.sender /= recipient.id) postbox)
                                                          sender
                                                          recipient

-- | Same as `computeMessage` except doesn't filter the given postbox for messages that don't come from the
-- recipient. Hence should only be used when it is known that none of the messages in the postbox come
-- from the recipient.
computeMessage' :: (ValuationFamily v, Var a)
    => [DMP.Message (v a)]
    -> DMP.NodeWithPid (v a)
    -> DMP.NodeWithPid (v a)
    -> v a
computeMessage' postbox sender recipient = project (combines1 (sender.node.v : map (.msg) postbox))
                                                   (intersection sender.node.d recipient.node.d)

