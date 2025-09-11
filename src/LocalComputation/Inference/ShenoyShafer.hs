{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LocalComputation.Inference.ShenoyShafer (
      queries
    , InferredData
) where

import           Control.Distributed.Process                 hiding (Message)

import           Data.Set                                    (intersection)


import           Control.Exception                           (assert)
import qualified Data.List                                   as L
import           Data.Maybe                                  (fromJust)
import           LocalComputation.Inference.JoinTree         (Node (..),
                                                              baseJoinTree)
import qualified LocalComputation.Inference.JoinTree         as J
import qualified LocalComputation.Inference.JoinTree         as JT
import qualified LocalComputation.Inference.JoinTree.Diagram as D
import qualified LocalComputation.Inference.MessagePassing   as MP
import           LocalComputation.ValuationAlgebra

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
type InferredData v a = JT.JoinTree (v a)

-- | Extracts a given query from the query results.
--
-- Assumes query is subset of the domain the given valuations cover.
extractQueryResult :: forall v a. (Valuation v, Var a)
    => [Domain a]
    -> InferredData v a
    -> [v a]
extractQueryResult queryDomains results = map f queryDomains
    where
        -- Find valuation with domain equal to query then get the valuation
        f :: Domain a -> v a
        f q = (.v) $ fromJust $ L.find (\n -> q == n.d) (JT.vertexList results)


-- | Performs shenoy shafer inference.
--
-- Assumes query is subset of the domain the given valuations cover.
queries :: (MP.SerializableValuation v a, Show (v a))
    => D.DrawSettings
    -> [v a]
    -> [Domain a]
    -> Process [v a]
queries settings vs queryDomains = do
    drawTree settings.beforeInference treeBeforeInference

    treeAfterInference <- MP.messagePassing treeBeforeInference nodeActions

    drawTree settings.afterInference treeAfterInference

    pure $ extractQueryResult queryDomains treeAfterInference

    where
        treeBeforeInference = baseJoinTree vs queryDomains

        drawTree Nothing         _    = pure ()
        drawTree (Just filename) tree = liftIO $ D.draw filename tree


nodeActions :: (MP.SerializableValuation v a)
    => MP.NodeActions v a
nodeActions this neighbours resultPort = do

    collectResults    <- MP.collect                   this neighbours computeMessage'
    distributeResults <- MP.distribute collectResults this neighbours computeMessage

    -- Send result back to parent process
    let result = combines1 (this.node.v : map (.msg) distributeResults.postbox)
    assert (this.node.d == label result) (pure ())
    sendChan resultPort $ J.changeContent this.node result

-- | Computes a message to send to the given neighbour.
--
-- Computing this message consists of:
--  1. combining all messages in the sender's postbox that don't come from the neighbour
--  2. combining this result with the sender's valuation
--  3. projecting the result to the intersection of the sender and neighbour's domain
computeMessage :: (Valuation v, Var a)
    => [MP.Message (v a)]
    -> MP.NodeWithPid (v a)
    -> MP.NodeWithPid (v a)
    -> v a
computeMessage postbox sender recipient = computeMessage' (filter (\msg -> msg.sender /= recipient.id) postbox)
                                                          sender
                                                          recipient

-- | Same as `computeMessage` except doesn't filter the given postbox for messages that don't come from the
-- recipient. Hence should only be used when it is known that none of the messages in the postbox come
-- from the recipient.
computeMessage' :: (Valuation v, Var a)
    => [MP.Message (v a)]
    -> MP.NodeWithPid (v a)
    -> MP.NodeWithPid (v a)
    -> v a
computeMessage' postbox sender recipient = project (combines1 (sender.node.v : map (.msg) postbox))
                                                   (intersection sender.node.d recipient.node.d)

