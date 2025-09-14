module LocalComputation.Inference.MessagePassing.Threads (

) where
import qualified Data.Map                                   as M
import qualified LocalComputation.Inference.JoinTree        as JT
import qualified LocalComputation.Inference.JoinTree.Forest as JT hiding
                                                                  (unsafeUpdateValuations)
import qualified LocalComputation.Inference.JoinTree.Tree   as JT
import qualified LocalComputation.Utils                     as U
import qualified LocalComputation.ValuationAlgebra          as V

messagePassing :: V.ValuationFamily v
    => JT.JoinForest (v a)
    -> JT.JoinForest (v a)
messagePassing tree = undefined

-- TODO: If the constant recalculation of roots is showing poor upon profiling
-- consider fixing this function rather than reimplementing the join tree (for now).
collect :: (V.ValuationFamily v, V.Var a) => JT.JoinTree (v a) -> JT.JoinTree (v a)
collect tree = JT.unsafeUpdateValuations newNodes tree
    where
        -- TODO: parallelise

        -- Computes collect on all subtrees
        subTrees = fmap collect $ JT.subTrees tree
        subTreeRootVs = fmap (.root.v) subTrees

        -- TODO: Doesn't project.

        -- Creates new root using neighbours who finished collect
        newRoot = tree.root { JT.v = V.combines1 (tree.root.v : subTreeRootVs) }

        -- Creates map from old nodes to updated ones
        newNodes = foldr (M.union . JT.toValuationMap) (M.singleton newRoot.id newRoot.v) subTrees

-- | Takes a tree that has had collect performed on it, and returns the tree after distribute has been performed.
distribute :: (V.ValuationFamily v, V.Var a) => JT.JoinTree (v a) -> JT.JoinTree (v a)
distribute tree = undefined

distribute' :: (V.ValuationFamily v, V.Var a) => v a -> JT.JoinTree (v a) -> JT.JoinTree (v a)
distribute' receieved tree = undefined

    where
        -- Computes distribute on all subtrees
        subTrees = JT.subTrees tree

        subTreeRootVs = fmap (.root.v) subTrees







