{-# LANGUAGE ScopedTypeVariables #-}

module LocalComputation.Inference.MessagePassing.Threads (
      messagePassing
    , messagePassing'
    , collectAndCalculate
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
import qualified Data.List                                  as L
import qualified Data.Tuple.Extra                           as B
import qualified LocalComputation.Utils                     as U

-- TODO: For performance;
--  1. Profile to investigate whats taking the most time
--  2. consider sparks
--  3. consider if we are spending all our time garbage collecting (does this show in the profiler?)
--          (see 40:45 of https://www.youtube.com/watch?v=trDqqZldxQA)

--------------------------------------------------------------------------------
-- Full message propagation (answering all queries)
--------------------------------------------------------------------------------
-- | Performs message passing over the given forest.
--
-- Only query nodes will have their valuations updated.
messagePassing :: (P.NFData (v a), V.ValuationFamily v, V.Var a)
    => JT.JoinForest (v a)
    -> JT.JoinForest (v a)
-- TODO: parallelise
messagePassing t = JT.unsafeToForest' $ map messagePassing' $ JT.treeList t

messagePassing' :: (P.NFData (v a), V.ValuationFamily v, V.Var a)
    => JT.JoinTree (v a)
    -> JT.JoinTree (v a)
messagePassing' t
    | JT.hasQueryNode t = calculate . distribute . collect $ t
    | otherwise         = t

-- TODO: It does seem like we are copying the tree alot of times; but i don't know whats going on under the hood.
-- Additionally it seems like we update all postboxes over and over when we really want something thats
-- more like 'attatching the current node to the updated tree'.

-- | Performs collect on the given tree.
collect :: (P.NFData (v a), V.ValuationFamily v, V.Var a) => JT.JoinTree (v a) -> JT.JoinTree (v a)
collect tree = JT.unsafeUpdatePostboxes newPostboxes tree
    where
        -- Computes collect on all subtrees, filling their postboxes
        -- subTrees = P.parMap P.rdeepseq collect $ JT.subTrees tree
        subTrees = -- P.withStrategy (P.parListChunk 3 P.rdeepseq)
                                  (fmap collect $ JT.subTrees tree)

        -- Get message for us from each subtree
        subTreeMessages = map (\subTree -> (subTree.root.id, messageForNode tree.root subTree.root)) subTrees

        -- Creates new root filling postbox with messages
        newRoot = tree.root { JT.postbox = Just $ M.fromList subTreeMessages }

        -- Creates a mapping from ids to postboxes
        -- (used for updating nodes in the original tree)
        newPostboxes = foldr (M.union . JT.toPostboxMap) (M.singleton newRoot.id newRoot.postbox) subTrees

-- | Takes a tree that has had collect performed on it, and returns the tree after distribute has been performed.
--
-- __Warning__: Assumes collect has been performed on the tree.
distribute :: (P.NFData (v a), V.ValuationFamily v, V.Var a) => JT.JoinTree (v a) -> JT.JoinTree (v a)
distribute tree | assert (JT.verticesHavePostboxes tree) False = undefined
distribute tree = distribute' Nothing tree

distribute' :: (P.NFData (v a), V.ValuationFamily v, V.Var a) => Maybe (JT.Id, v a) -> JT.JoinTree (v a) -> JT.JoinTree (v a)
distribute' incoming tree = JT.unsafeUpdatePostboxes newPostboxes tree

    where
        -- Insert the new message into the nodes postbox
        newRoot = case incoming of
                    Nothing             -> tree.root
                    Just (sender, sent) -> tree.root { JT.postbox = fmap (M.insert sender sent) tree.root.postbox }

        -- The subtrees, sorted by length so that the largest
        -- tree gets evaluated first during the foldr later on.
        subTrees = L.sortOn JT.vertexCount $ JT.subTrees tree

        -- Compute messages to send to subtrees
        subTreeMessages = fmap (\subTree -> Just (newRoot.id, messageForNode subTree.root newRoot)) subTrees

        distributed = parSome results toPar
            where
                results = fmap (uncurry distribute') $ zip subTreeMessages subTrees

                toPar = map (\t -> False) subTrees


        -- Creates a mapping from ids to postboxes
        -- (used for updating nodes in the original tree)
        newPostboxes = foldr (M.union . JT.toPostboxMap) (M.singleton newRoot.id newRoot.postbox) distributed

parSome :: (P.NFData a) => [a] -> [Bool] -> [a]
parSome xs parStates = P.withStrategy (P.parListN (length shouldPar) P.rdeepseq)
                                      (shouldPar ++ shouldNotPar)
    where
        (shouldPar, shouldNotPar) = B.both (map fst) $ L.partition snd $ zip xs parStates

messageForNode :: (V.ValuationFamily v, V.Var a) => JT.Node (v a) -> JT.Node (v a) -> v a
messageForNode receiver sender = V.project (V.combines1 (sender.v : senderPostbox))
                                           (S.intersection receiver.d sender.d)
    where
        senderPostbox = M.elems . M.filterKeys (/= receiver.id) . fromJust $ sender.postbox

-- | Takes a tree that has had collect and then distribute performed on it, and returns the tree
-- after computing the resulting valuations of each query node.
--
-- __Warning__: Does not compute the resulting valuations for non-query nodes. Assumes collect and distribute
-- have been performed on the given tree.
calculate :: forall v a . (P.NFData (v a), V.ValuationFamily v, V.Var a) => JT.JoinTree (v a) -> JT.JoinTree (v a)
calculate tree = JT.unsafeUpdateValuations mapping tree
    where
        queryNodes = filter (\n -> n.t == JT.Query) $ JT.vertexList tree
        newValuations = P.parMap P.rdeepseq updatedValuation queryNodes
        -- newValuations = fmap updatedValuation queryNodes
        mapping = M.fromList $ zipWith (\n v -> (n.id, v)) queryNodes newValuations

        updatedValuation :: JT.Node (v a) -> v a
        updatedValuation n = V.combines1 (n.v : postbox)
            where
                postbox = M.elems . fromJust $ n.postbox

--------------------------------------------------------------------------------
-- Partial message propagation (answering one query)
--------------------------------------------------------------------------------
-- TODO: Test elimination vs projection here; does it make a difference?
collectAndCalculate :: forall v a . (P.NFData (v a), V.ValuationFamily v, V.Var a)
    => JT.JoinTree (v a) -> JT.JoinTree (v a)
collectAndCalculate tree = JT.unsafeUpdateValuations newValuations tree
    where
        -- Computes collect on all subtrees, filling their postboxes
        -- subTrees = P.parMap P.rdeepseq collect $ JT.subTrees tree
        subTrees = fmap collectAndCalculate $ JT.subTrees tree

        -- Get message for us from each subtree
        subTreeMessages = map (\subTree -> (subTree.root.id, messageForThis tree.root subTree.root)) subTrees

        -- Creates new root by updating valuation on current root
        newRoot = tree.root { JT.v = V.combines1 (tree.root.v : map snd subTreeMessages) }

        -- Creates a mapping from ids to postboxes
        -- (used for updating nodes in the original tree)
        newValuations = foldr (M.union . JT.toValuationMap) (M.singleton newRoot.id newRoot.v) subTrees

        messageForThis :: JT.Node (v a) -> JT.Node (v a) -> v a
        messageForThis this sender = V.project sender.v (S.intersection this.d sender.d)

-- TODO: Remove. Tested, and makes no difference.

-- | __Warning__: Assumes 'd' is a subset of 'label v'
projectByElimination :: (V.Valuation v a) => v a -> V.Domain a -> v a
projectByElimination v d
    | S.size diff > 0 = projectByElimination (V.project v smallerDomain) d
    | otherwise       = v
    where

        diff = S.difference (V.label v) d

        elementInDiff = S.findMin diff

        smallerDomain = S.delete elementInDiff (V.label v)

