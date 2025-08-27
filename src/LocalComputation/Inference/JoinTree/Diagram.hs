{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

module LocalComputation.Inference.JoinTree.Diagram (
    draw
) where

import           Diagrams.Backend.SVG                (renderSVG)
import           Diagrams.Backend.SVG.CmdLine
import           Diagrams.Prelude

import qualified Algebra.Graph                       as G
import           Control.Exception                   (assert)
import qualified Data.List                           as L
import           Data.Maybe                          (fromJust)
import qualified LocalComputation.Inference.JoinTree as JT

myCircle :: Diagram B
myCircle = circle 1

-- TODO: A different data structure could be better here. Additionally, we might want
-- to unite all graphs under one data structure to prevent confusion.

draw :: FilePath -> G.Graph (JT.Node a) -> IO ()
draw name g = renderSVG name (dims2D 700 700) (tree g)

-- | Assumes a tree like structure, and that the node with the highest `id` is the root.
tree :: G.Graph (JT.Node a) -> Diagram B
tree g = assert rootHasNoOutgoingEdges $ tree' root g
    where
        root = L.maximumBy (\x y -> x.id `compare` y.id) $ G.vertexList g

        tree' _ _ = circle 1 ||| text "foobar" ||| text "fooooooooo of barssssss" ||| text "foo"

        rootHasNoOutgoingEdges = length rootOutgoingEdges == 0
        rootOutgoingEdges = snd . fromJust . L.find (\(x, adjacents) -> x.id == root.id) . G.adjacencyList $ g

-- | Assumes the given node is in the tree.
tree' :: JT.Node a -> G.Graph (JT.Node a) -> Diagram B
tree' node g = vsep 3 [root, parents] # applyAll (map (\parent -> connectOutside parent.id node.id) incoming)
    where
        root = text (show node.id) <> circle 1 # named node.id

        -- Horribly inefficent, constantly recalculated
        incoming = snd . fromJust . L.find (\(x, adjacents) -> x.id == node.id) . G.adjacencyList . G.transpose $ g

        parents = centerX . hsep 1 . map alignT . map (\n -> tree' n g) $ incoming
