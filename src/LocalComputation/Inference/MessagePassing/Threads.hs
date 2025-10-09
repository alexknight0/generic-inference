{-# LANGUAGE CPP                 #-}
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
import qualified Data.IORef                                 as IO
import qualified Data.List                                  as L
import qualified Data.Tree                                  as T
import qualified Data.Tuple.Extra                           as B
import qualified LocalComputation.Utils                     as U
import           Numeric.Natural
import qualified System.IO.Unsafe                           as IO

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
    | JT.hasQueryNode t = calculate . JT.unsafeFromTree . distribute' Nothing . collect' . JT.toTree . JT.tracking $ t
    | otherwise         = t

-- TODO: It does seem like we are copying the tree alot of times; but i don't know whats going on under the hood.
-- Additionally it seems like we update all postboxes over and over when we really want something thats
-- more like 'attatching the current node to the updated tree'.

-- | Performs collect on the given tree.
collect :: (P.NFData (v a), V.ValuationFamily v, V.Var a) => JT.JoinTree (v a) -> JT.JoinTree (v a)
collect = JT.unsafeFromTree . collect' . JT.toTree

collect' :: (P.NFData (v a), V.ValuationFamily v, V.Var a) => T.Tree (JT.Node (v a)) -> T.Tree (JT.Node (v a))
collect' (T.Node root subTrees) = T.Node newRoot newSubTrees
    where
        -- Computes collect on all subtrees, filling their postboxes
        -- subTrees = P.parMap P.rdeepseq collect $ JT.subTrees tree
        newSubTrees = -- P.withStrategy (P.parListChunk 3 P.rdeepseq)
                                (fmap collect' subTrees)

        -- Get message for us from each subtree
        subTreeMessages = map (\subTree -> (subTree.rootLabel.id, messageForNode root subTree.rootLabel)) newSubTrees

        -- Creates new root filling postbox with messages
        newRoot = root { JT.postbox = Just $ M.fromList subTreeMessages }

-- | Takes a tree that has had collect performed on it, and returns the tree after distribute has been performed.
--
-- __Warning__: Assumes collect has been performed on the tree.
distribute :: (P.NFData (v a), V.ValuationFamily v, V.Var a) => JT.JoinTree (v a) -> JT.JoinTree (v a)
distribute tree | assert (JT.verticesHavePostboxes tree) False = undefined
distribute tree = JT.unsafeFromTree $ distribute' Nothing (JT.toTree tree)

distribute' :: (P.NFData (v a), V.ValuationFamily v, V.Var a) => Maybe (JT.Id, v a) -> T.Tree (JT.Node (v a)) -> T.Tree (JT.Node (v a))
distribute' incoming (T.Node root subTrees) = T.Node newRoot newSubTrees

    where
        -- Insert the new message into the nodes postbox
        newRoot = case incoming of
                    Nothing             -> root
                    Just (sender, sent) -> root { JT.postbox = fmap (M.insert sender sent) root.postbox }

        -- The subtrees, sorted by length so that the largest
        -- tree gets evaluated first during the foldr later on.
        -- DEPRECATED as we don't parallelise currently.
        sortedSubTrees = subTrees -- L.sortOn JT.vertexCount subTrees

        -- Compute messages to send to subtrees
        subTreeMessages = fmap (\subTree -> Just (newRoot.id, messageForNode subTree.rootLabel newRoot)) sortedSubTrees

        newSubTrees = parSome results toPar
            where
                results = fmap (uncurry distribute') $ zip subTreeMessages sortedSubTrees

                toPar = map (\_ -> False) sortedSubTrees

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
collectAndCalculate :: forall v a . (P.NFData (v a), V.ValuationFamily v, V.Var a)
    => JT.JoinTree (v a) -> JT.JoinTree (v a)
collectAndCalculate = JT.unsafeFromTree . collectAndCalculate' . JT.toTree . JT.tracking

collectAndCalculate' :: forall v a . (P.NFData (v a), V.ValuationFamily v, V.Var a)
    => T.Tree (JT.Node (v a)) -> T.Tree (JT.Node (v a))
collectAndCalculate' (T.Node root subTrees) = T.Node newRoot newSubTrees
    where
        -- Computes collect on all subtrees, filling their postboxes
        -- subTrees = P.parMap P.rdeepseq collect $ JT.subTrees tree
        newSubTrees = fmap collectAndCalculate' subTrees

        -- Get message for us from each subtree
        subTreeMessages = map (\subTree -> (subTree.rootLabel.id, messageForThis root subTree.rootLabel)) newSubTrees

        -- Creates new root by updating valuation on current root
        newRoot = root { JT.v = V.combines1 (root.v : map snd subTreeMessages) }

        messageForThis :: JT.Node (v a) -> JT.Node (v a) -> v a
        messageForThis this sender = V.project sender.v (S.intersection this.d sender.d)


