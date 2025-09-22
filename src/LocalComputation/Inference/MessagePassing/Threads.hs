{-# LANGUAGE ScopedTypeVariables #-}

module LocalComputation.Inference.MessagePassing.Threads (
    messagePassing
) where
import           Control.Exception                          (assert)
import qualified Data.Map                                   as M
import           Data.Maybe                                 (fromJust)
import qualified Data.Set                                   as S
import qualified LocalComputation.Inference.JoinTree        as JT
import qualified LocalComputation.Inference.JoinTree.Forest as JT hiding
                                                                  (unsafeUpdateValuations,
                                                                   vertexList)
import qualified LocalComputation.Inference.JoinTree.Tree   as JT
import qualified LocalComputation.Utils                     as M (filterKeys)
import qualified LocalComputation.ValuationAlgebra          as V

import qualified Control.Parallel.Strategies                as P

-- TODO: For performance;
--  1. Profile to investigate whats taking the most time
--  2. consider sparks
--  3. consider if we are spending all our time garbage collecting (does this show in the profiler?)
--          (see 40:45 of https://www.youtube.com/watch?v=trDqqZldxQA)

messagePassing :: (P.NFData (v a), V.ValuationFamily v, V.Var a)
    => JT.JoinForest (v a)
    -> JT.JoinForest (v a)
-- TODO: parallelise
messagePassing t = JT.unsafeToForest' $ map messagePassing' $ JT.treeList t

messagePassing' :: (P.NFData (v a), V.ValuationFamily v, V.Var a)
    => JT.JoinTree (v a)
    -> JT.JoinTree (v a)
messagePassing' = calculate . distribute . collect

-- TODO: If the constant recalculation of roots is showing poor upon profiling
-- consider fixing this function rather than reimplementing the join tree (for now).
collect :: forall v a . (P.NFData (v a), V.ValuationFamily v, V.Var a) => JT.JoinTree (v a) -> JT.JoinTree (v a)
collect tree = JT.unsafeUpdatePostboxesAndCache newPostboxesAndCaches tree
    where
        -- Computes collect on all subtrees, filling their postboxes
        subTrees = P.parMap P.rdeepseq collect $ JT.subTrees tree

        -- Get message for us from each subtree
        subTreeMessages = map toMessage subTrees
            where
                toMessage :: JT.JoinTree (v a) -> MsgResults (v a)
                toMessage subTree = messageForNode2 tree.root subTree.root

        newSubTrees = zipWith JT.unsafeUpdateRootCache subTrees (map (.newSenderCache) subTreeMessages)

        -- Creates new root filling postbox with messages
        newRoot = tree.root { JT.postbox = Just $ M.fromList $ map (\m -> (m.sender, m.msg)) subTreeMessages }

        -- Creates a mapping from ids to the new postboxes and caches
        -- (used for updating the original tree)
        newPostboxesAndCaches = foldr (M.union . JT.toPostboxAndCacheMap)
                                      (M.singleton newRoot.id (newRoot.postbox, newRoot.cache))
                                      newSubTrees

-- | Takes a tree that has had collect performed on it, and returns the tree after distribute has been performed.
--
-- __Warning__: Assumes collect has been performed on the tree.
distribute :: (P.NFData (v a), V.ValuationFamily v, V.Var a) => JT.JoinTree (v a) -> JT.JoinTree (v a)
distribute tree | assert (JT.verticesHavePostboxes tree) False = undefined
distribute tree = distribute' Nothing tree

distribute' :: forall v a . (P.NFData (v a), V.ValuationFamily v, V.Var a) => Maybe (JT.Id, v a) -> JT.JoinTree (v a) -> JT.JoinTree (v a)
distribute' incoming tree = JT.unsafeUpdatePostboxesAndCache newPostboxesAndCaches tree

    where
        -- Insert the new message into the nodes postbox
        rootWithPostbox = case incoming of
                    Nothing             -> tree.root
                    Just (sender, sent) -> tree.root { JT.postbox = fmap (M.insert sender sent) tree.root.postbox }

        subTrees = JT.subTrees tree

        getMessageToSend :: JT.JoinTree (v a) -> (JT.Node (v a), [v a]) -> (JT.Node (v a), [v a])
        getMessageToSend subTree (sender, msgs) = (sender { JT.cache = result.newSenderCache }, result.msg : msgs)
            where
                result = messageForNode2 subTree.root sender

        -- Compute messages to send to subtrees
        (newRoot, subTreeMessages) = foldr getMessageToSend (rootWithPostbox, []) subTrees

        -- Computes distribute on all subtrees
        distributed = P.parMap P.rdeepseq (uncurry distribute') $ zip subTreeMessagesWithId subTrees
            where
                -- Reformat messages for use with distribute
                subTreeMessagesWithId = map (\m -> Just (newRoot.id, m)) subTreeMessages

        -- Creates a mapping from ids to the new postboxes and caches
        -- (used for updating the original tree)
        newPostboxesAndCaches = foldr (M.union . JT.toPostboxAndCacheMap)
                                      (M.singleton newRoot.id (newRoot.postbox, newRoot.cache))
                                      distributed

messageForNode :: (V.ValuationFamily v, V.Var a) => JT.Node (v a) -> JT.Node (v a) -> v a
messageForNode receiver sender = V.project (V.combines1 (sender.v : postbox))
                                           (S.intersection receiver.d sender.d)
    where
        postbox = M.elems . M.filterKeys (/= receiver.id) . fromJust $ sender.postbox

data MsgResults v = MsgResults { newSenderCache :: JT.Cache v, sender :: JT.Id, msg :: v }

messageForNode2 :: (V.ValuationFamily v, V.Var a) => JT.Node (v a) -> JT.Node (v a) -> MsgResults (v a)
messageForNode2 receiver sender = MsgResults { newSenderCache = newSenderCache
                                             , sender = sender.id
                                             , msg = V.project (combined)
                                                                   (S.intersection receiver.d sender.d)
                                            }
    where
        postbox = M.toList . M.filterKeys (/= receiver.id) . fromJust $ sender.postbox
        ids = map fst postbox ++ [sender.id]
        vs = map snd postbox ++ [sender.v]

        newSenderCache = cachedCombine sender.cache ids vs
        combined = fromJust $ JT.lookupCache ids newSenderCache

cachedCombine :: (V.Valuation v a) => JT.Cache (v a) -> [JT.Id] -> [v a] -> JT.Cache (v a)
cachedCombine c []        _             = c
cachedCombine c _         []            = c
cachedCombine c ids@(_ : ids') (v : vs) = case JT.lookupCache ids c of
                                                Just _  -> c
                                                Nothing -> JT.updateCache newCache ids (V.combine v vsCombined)
    where
        newCache = cachedCombine c ids' vs

        vsCombined = fromJust $ JT.lookupCache ids' newCache

-- cachedCombine :: JT.Cache (v a) -> [(JT.Id, v a)] -> (JT.Cache (v a), v a)
-- cachedCombine c [(i, v)]      = (c, v)
-- cachedCombine c ((i, v) : vs)
--     | Just result <- JT.lookupCache ids c = (c, result)
--     | otherwise = (JT.updateCache (i : ids)
--     where
--         ids = map fst vs
--
--         (newCache, result) = cachedCombine c vs


-- | Takes a tree that has had collect and then distribute performed on it, and returns the tree
-- after computing the resulting valuations of each node.
calculate :: forall v a . (P.NFData (v a), V.ValuationFamily v, V.Var a) => JT.JoinTree (v a) -> JT.JoinTree (v a)
calculate tree = JT.unsafeUpdateValuations mapping tree
    where
        vertices = JT.vertexList tree
        newValuations = P.parMap P.rdeepseq updatedValuation vertices
        mapping = M.fromList $ zipWith (\n v -> (n.id, v)) vertices newValuations

        updatedValuation :: JT.Node (v a) -> v a
        updatedValuation n = V.combines1 (n.v : postbox)
            where
                postbox = M.elems . fromJust $ n.postbox


