{-# LANGUAGE ScopedTypeVariables #-}

module LocalComputation.Inference.MessagePassing.Threads (

) where
import           Control.Exception                          (assert)
import qualified Data.Map                                   as M
import           Data.Maybe                                 (fromJust)
import qualified Data.Set                                   as S
import qualified LocalComputation.Inference.JoinTree        as JT
import qualified LocalComputation.Inference.JoinTree.Forest as JT hiding
                                                                  (unsafeUpdateValuations)
import qualified LocalComputation.Inference.JoinTree.Tree   as JT
import qualified LocalComputation.Utils                     as M (filterKeys)
import qualified LocalComputation.Utils                     as U
import qualified LocalComputation.ValuationAlgebra          as V

messagePassing :: (V.ValuationFamily v, V.Var a)
    => JT.JoinForest (v a)
    -> JT.JoinForest (v a)
-- TODO: parallelise
messagePassing t = JT.unsafeToForest' $ map messagePassing' $ JT.treeList t

messagePassing' :: (V.ValuationFamily v, V.Var a)
    => JT.JoinTree (v a)
    -> JT.JoinTree (v a)
messagePassing' = calculate . distribute . collect

-- TODO: If the constant recalculation of roots is showing poor upon profiling
-- consider fixing this function rather than reimplementing the join tree (for now).
collect :: (V.ValuationFamily v, V.Var a) => JT.JoinTree (v a) -> JT.JoinTree (v a)
collect tree = JT.unsafeUpdatePostboxes newNodes tree
    where
        -- TODO: parallelise

        -- Computes collect on all subtrees, filling their postboxes
        subTrees = fmap collect $ JT.subTrees tree

        -- Get message for us from each subtree
        subTreeMessages = map (\subTree -> (subTree.root.id, messageForNode tree.root subTree.root)) subTrees

        -- Creates new root filling postbox with messages
        newRoot = tree.root { JT.postbox = Just $ M.fromList subTreeMessages }

        -- Creates a mapping an id to the new postbox of that node
        newNodes = foldr (M.union . JT.toPostboxMap) (M.singleton newRoot.id newRoot.postbox) subTrees

-- | Takes a tree that has had collect performed on it, and returns the tree after distribute has been performed.
--
-- __Warning__: Assumes collect has been performed on the tree.
distribute :: (V.ValuationFamily v, V.Var a) => JT.JoinTree (v a) -> JT.JoinTree (v a)
distribute tree | assert (JT.verticesHavePostboxes tree) False = undefined
distribute tree = distribute' Nothing tree

distribute' :: (V.ValuationFamily v, V.Var a) => Maybe (JT.Id, v a) -> JT.JoinTree (v a) -> JT.JoinTree (v a)
distribute' incoming tree = JT.unsafeUpdatePostboxes newNodes tree

    where
        -- Insert the new message into the nodes postbox
        newRoot = case incoming of
                    Nothing             -> tree.root
                    Just (sender, sent) -> tree.root { JT.postbox = fmap (M.insert sender sent) tree.root.postbox }

        subTrees = JT.subTrees tree

        -- Compute messages to send to subtrees
        subTreeMessages = fmap (\subTree -> Just (newRoot.id, messageForNode subTree.root newRoot)) subTrees

        -- Computes distribute on all subtrees
        -- TODO: parallelise
        distributed = zipWith distribute' subTreeMessages subTrees

        -- Creates a mapping from the old nodes to the new updated ones
        newNodes = foldr (M.union . JT.toPostboxMap) (M.singleton newRoot.id newRoot.postbox) distributed

messageForNode :: (V.ValuationFamily v, V.Var a) => JT.Node (v a) -> JT.Node (v a) -> v a
messageForNode receiver sender = V.project (V.combines1 (sender.v : postbox))
                                           (S.intersection receiver.d sender.d)
    where
        postbox = M.elems . M.filterKeys (/= receiver.id) . fromJust $ sender.postbox

-- | Takes a tree that has had collect and then distribute performed on it, and returns the tree
-- after computing the resulting valuations of each node.
calculate :: forall v a . (V.ValuationFamily v, V.Var a) => JT.JoinTree (v a) -> JT.JoinTree (v a)
-- TODO: parallelise
calculate tree = JT.unsafeUpdateValuations newValuations tree
    where
        newValuations = M.map updatedValuation $ JT.toMap tree

        updatedValuation :: JT.Node (v a) -> v a
        updatedValuation n = V.combines1 (n.v : postbox)
            where
                postbox = M.elems . fromJust $ n.postbox


