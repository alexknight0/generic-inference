{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LocalComputation.Inference.ShenoyShafer (
      shenoyJoinTree
    , answerQueriesM, answerQueryM, answerQueriesDrawGraphM
    , answerQueries, answerQuery
    , inference
    , answerQuery'
    , InferredData
) where

import           Control.Distributed.Process                 hiding (Message)

import qualified Algebra.Graph                               as DG
import qualified Algebra.Graph.Undirected                    as UG
import           Data.Set                                    (intersection,
                                                              isSubsetOf)


import           Control.Exception                           (assert)
import qualified Data.List                                   as L
import           LocalComputation.Inference.JoinTree         (Node (..),
                                                              baseJoinTree)
import qualified LocalComputation.Inference.JoinTree         as J
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

type InferredData v a = DG.Graph (Node (v a))

-- TODO: safely handle invalid queries?
answerQueries :: forall v a. (Valuation v, Var a)
    => [Domain a]
    -> InferredData v a
    -> [v a]
answerQueries queryDomains results = map queryToAnswer queryDomains
    where
        queryToAnswer :: Domain a -> v a
        queryToAnswer d = tmp.v
            where
                tmp = case L.find (\n -> d == n.d) (DG.vertexList results) of
                            Just x -> x
                            Nothing -> -- pTraceShow (showDomain d, map (showDomain . (.d)) $ DG.vertexList results, map showDomain queryDomains) $
                                        error "Find failed."


-- TODO: safely handle invalid queries?
answerQueries' :: forall v a. (Valuation v, Var a)
    => [Domain a]
    -> InferredData v a
    -> [v a]
answerQueries' queryDomains results = map queryToAnswer queryDomains
    where
        queryToAnswer :: Domain a -> v a
        queryToAnswer d = project closest.v d
            where
                closest = head $ L.sortOn (\n -> length n.d) $ filter (\n -> d `isSubsetOf` n.d) (DG.vertexList results)

                -- tmp = case L.find (\n -> d `isSubsetOf` n.d) (DG.vertexList results) of
                --             Just x -> x
                --             Nothing -> -- pTraceShow (showDomain d, map (showDomain . (.d)) $ DG.vertexList results, map showDomain queryDomains) $
                --                         error "Find failed."

-- TODO safely handle invalid queries?
answerQuery' :: forall v a. (Valuation v, Var a)
    => Domain a
    -> InferredData v a
    -> v a
answerQuery' q results = head $ answerQueries' [q] results

answerQuery :: forall v a. (Valuation v, Var a)
    => Domain a
    -> InferredData v a
    -> v a
answerQuery q results = head $ answerQueries [q] results

answerQueriesM :: (MP.SerializableValuation v a)
    => [v a]
    -> [Domain a]
    -> Process [v a]
answerQueriesM vs queryDomains = do
    results <- MP.messagePassing (baseJoinTree vs queryDomains) nodeActions
    pure $ answerQueries queryDomains results

answerQueryM :: (MP.SerializableValuation v a)
    => [v a]
    -> Domain a
    -> Process (v a)
answerQueryM vs q = do
    results <- MP.messagePassing (baseJoinTree vs [q]) nodeActions
    pure $ answerQuery q results

-- TODO: Right now visualises the after tree. We should have options for both i guess!
answerQueriesDrawGraphM :: (MP.SerializableValuation v a, Show (v a))
    => FilePath
    -> [v a]
    -> [Domain a]
    -> Process [v a]
answerQueriesDrawGraphM filename vs queryDomains = do
    let tree = baseJoinTree vs queryDomains
    liftIO $ D.draw filename tree
    results <- MP.messagePassing tree nodeActions
    -- liftIO $ D.draw filename results
    pure $ answerQueries queryDomains results

inference :: (MP.SerializableValuation v a)
    => [v a]
    -> [Domain a]
    -> Process (InferredData v a)
inference vs queryDomains = MP.messagePassing (baseJoinTree vs queryDomains) nodeActions

-- TODO: Is `shenoyJoinTree` still used?

-- The base join tree must be transformed to an undirected graph.
-- While mailboxes should be connected up for each neighbour, this happens in the
-- 'MP.messagePassing' function which also handles starting the message passing.
shenoyJoinTree :: forall v a. (Valuation v, Var a)
    => [v a]
    -> [Domain a]
    -> UG.Graph (Node (v a))
shenoyJoinTree vs queryDomains = UG.toUndirected (baseJoinTree vs queryDomains)

nodeActions :: (MP.SerializableValuation v a)
    => MP.NodeActions v a
nodeActions this neighbours resultPort = do

    collectResults    <- MP.collect                   this neighbours computeMessage'
    distributeResults <- MP.distribute collectResults this neighbours computeMessage

    -- Send result back to parent process
    let result = combines1 (this.node.v : map (.msg) distributeResults.postbox)
    assert (this.node.d == label result) (pure ())
    sendChan resultPort $ J.changeContent this.node result

-- TODO: Update
-- | Computes a message to send to the given neighbour.
--
-- Computing this message consists of:
--  1. combining all messages in the sender's postbox that don't come from the neighbour
--  2. combining this result with the sender's valuation
--  3. projecting the result to the intersection of the sender and neighbour's domain
computeMessage :: (Valuation v, Var a)
    => [MP.Message (v a)]
    -> MP.NodeWithProcessId (v a)
    -> MP.NodeWithProcessId (v a)
    -> v a
computeMessage postbox sender recipient = computeMessage' (filter (\msg -> msg.sender /= recipient.id) postbox)
                                                          sender
                                                          recipient

-- | Same as `computeMessage` except doesn't filter the given postbox for messages that don't come from the
-- recipient. Hence should only be used when it is known that none of the messages in the postbox come
-- from the recipient.
computeMessage' :: (Valuation v, Var a)
    => [MP.Message (v a)]
    -> MP.NodeWithProcessId (v a)
    -> MP.NodeWithProcessId (v a)
    -> v a
computeMessage' postbox sender recipient = project (combines1 (sender.node.v : map (.msg) postbox))
                                                   (intersection sender.node.d recipient.node.d)



------------------------------------------------------------------------------
-- Solution Construction                                                    --
------------------------------------------------------------------------------

-- configSet :: (Valuation v, Show a, Ord a)
--     => v a
--     -> Domain a
--     -> VariableArrangement v a b
--     -> Maybe (S.Set (VariableArrangement v a b))
-- configSet phi t x
--     | not $ S.isSubsetOf t (label phi) = Nothing
--     | otherwise = undefined



-- type SolutionSet v a = ConfigurationExtensionSet v a
--
-- -- | The configuration extension set.
-- --
-- -- This is detailed in page 294 of Marc Pouly's "Generic Inference". In short, this is an intermediate
-- -- product in a larger computation and is related to the set of variables that have not yet been assigned
-- -- values.
-- data ConfigurationExtensionSet v a = ConfigurationExtensionSet {
--           t   :: Domain a
--         , phi :: v a
--         , f   :: VariableArrangement a b -> S.Set (VariableArrangement a b)
--     }
--
-- instance (Valuation v, Ord a, Ord b, Show a, Show b) => HasField "s" (ConfigurationExtensionSet v a) (Domain a) where
--     getField w = label w.phi
--
-- -- | Compute all solutions.
-- --
-- -- Note this function does not require a complete run of the specifically the shenoy shafer architecture,
-- -- but rather any multi-query local computation architecture should suffice.
-- -- This algorithm is based off page 299 of Marc Pouly's "Generic Inference".
-- computeSolutions ::
--        InferredData v a
--     -> ConfigurationExtensionSet v a
-- computeSolutions = undefined
--
-- isValidConfigurationExtensionSet :: (Valuation v, Ord a, Ord b, Show a, Show b)
--     => ConfigurationExtensionSet v a
--     -> VariableArrangement a b
--     -> Bool
-- isValidConfigurationExtensionSet w x
--     -- Fits definition of configuration set from `t` to `s`
--     | not $ S.isSubsetOf w.t w.s = False
--     | not $ all elemOfOmegaSMinusT (w.f x) = False
--
--     where
--         elemOfOmegaSMinusT y = S.isSubsetOf (M.keysSet y) (S.difference w.s w.t)
--

--
-- instance (Valuation v, Ord a, Ord b, Show a, Show b) => HasField "s" (ConfigurationExtensionSet v a) (Domain a) where
--     getField w = label w.phi
--
-- -- TODO: Add the final property
--
-- isValidConfigurationExtensionSet :: (Valuation v, Ord a, Ord b, Show a, Show b)
--     => ConfigurationExtensionSet v a
--     -> VariableArrangement a b
--     -> Bool
-- isValidConfigurationExtensionSet w x
--     -- Fits definition of configuration set from `t` to `s`
--     | not $ S.isSubsetOf w.t w.s = False
--     | not $ all elemOfOmegaSMinusT (w.f x) = False
--     | otherwise = True
--
--     where
--         elemOfOmegaSMinusT y = S.isSubsetOf (Map.keysSet y) (S.difference w.s w.t)
--
-- | Compute all solutions.
--
-- Note this function does not require a complete run of the specifically the shenoy shafer architecture,
-- but rather any multi-query local computation architecture should suffice.
-- This algorithm is based off page 299 of Marc Pouly's "Generic Inference".
-- computeSolutions ::
--        [(Domain a, v a)]
--     -> ConfigurationExtensionSet v a
-- computeSolutions = undefined
