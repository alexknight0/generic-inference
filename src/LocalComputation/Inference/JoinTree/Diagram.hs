{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

module LocalComputation.Inference.JoinTree.Diagram (
    draw
) where

import           Diagrams.Backend.SVG                (renderSVG)
import           Diagrams.Backend.SVG.CmdLine
import           Diagrams.Prelude
import qualified Graphics.SVGFonts                   as SF

import qualified Algebra.Graph                       as G
import           Control.Exception                   (assert)
import qualified Data.List                           as L
import qualified Data.List.Extra                     as L (splitOn)
import           Data.Maybe                          (fromJust)
import qualified Debug.Trace                         as D
import qualified Graphics.SVGFonts.ReadFont          as SF
import qualified LocalComputation.Inference.JoinTree as JT

myCircle :: Diagram B
myCircle = circle 1

-- TODO: A different data structure could be better here. Additionally, we might want
-- to unite all graphs under one data structure to prevent confusion.

draw :: Show a => FilePath -> G.Graph (JT.Node a) -> IO ()
draw name g = tree g >>= renderSVG name (dims2D 1400 1400)

-- | Assumes a tree like structure, and that the node with the highest `id` is the root.
tree :: Show a => G.Graph (JT.Node a) -> IO (Diagram B)
tree g = do
    font <- SF.bit
    pure $ assert rootHasNoOutgoingEdges $ tree' font root g
    where
        root = L.maximumBy (\x y -> x.id `compare` y.id) $ G.vertexList g

        rootHasNoOutgoingEdges = length rootOutgoingEdges == 0
        rootOutgoingEdges = snd . fromJust . L.find (\(x, adjacents) -> x.id == root.id) . G.adjacencyList $ g

data TextWithBorder = TextWithBorder {
    diagram     :: Diagram B,
    borderWidth :: Double
}

textWithBorder :: (SF.PreparedFont Double) -> String -> TextWithBorder
textWithBorder font s = TextWithBorder ((textWithPadding <> rectangle) # withEnvelope rectangle)
                                       borderWidth
    where
        result = textWithNewlines font s # centerXY
        paddingSize = min (width result) (height result)
        borderWidth = 0.02 * paddingSize
        textWithPadding = result # frame (0.1 * paddingSize)

        rectangle = rect (width textWithPadding) (height textWithPadding) # lwL (borderWidth / 2) # dashingL [borderWidth * 2, borderWidth * 3] 0

textWithNewlines :: (SF.PreparedFont Double) -> String -> Diagram B
textWithNewlines font s = foldr ((===) . textWithEnvelope font) mempty $ L.splitOn "\n" s

textWithEnvelope :: (SF.PreparedFont Double) -> String -> Diagram B
textWithEnvelope font s = SF.svgText (SF.TextOpts font SF.HADV False) s # SF.set_envelope # lw none # fc black

-- | Assumes the given node is in the tree.
tree' :: Show a => (SF.PreparedFont Double) -> JT.Node a -> G.Graph (JT.Node a) -> Diagram B
tree' font node g = vsep vgap [root, parents] # applyAll arrows
    where
        -- root = text (show node.id) <> circle 1 # named node.id
        root = rootText.diagram # named node.id
        rootText = textWithBorder font (show node.v)

        -- Horribly inefficent, constantly recalculated
        incoming = snd . fromJust . L.find (\(x, adjacents) -> x.id == node.id) . G.adjacencyList . G.transpose $ g
        parents = centerX . hsep hgap . map alignT . map (\n -> tree' font n g) $ incoming

        arrows = map (\parent -> connectOutside' arrowOpts parent.id node.id # lwL (rootText.borderWidth / 1.5)) incoming
        arrowOpts = with & headLength .~ local (rootText.borderWidth * 4.5)
                         & headGap    .~ local (rootText.borderWidth * 6)
                         & tailGap    .~ local (rootText.borderWidth * 6)
        vgap = 0.5 * height root
        hgap = 0.25 * width root


exampleText2 =
 "var   A   B   Probability                                 \n\
 \                                                          \n\
 \ 0    0   0       0.7       P(var == 0 | A == 0 && B == 0)\n\
 \                                                          \n\
 \ 0    0   1       0.4       P(var == 0 | A == 0 && B == 1)\n\
 \                                                          \n\
 \ 0    0   2       0.3       P(var == 0 | A == 0 && B == 2)\n\
 \                                                          \n\
 \ 0    1   0       0.6       P(var == 0 | A == 1 && B == 0)\n"

exampleText = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nullam risus augue, porta vitae fringilla quis, laoreet sed dui. \nVivamus bibendum pharetra lectus ac fringilla. Praesent vitae bibendum lectus. Etiam ut felis ultrices, tincidunt dolor sagittis, posuere lectus. Nullam rhoncus fringilla sem, quis congue metus egestas sit amet. Nulla eget nunc sit amet dui tincidunt faucibus sodales eget diam. Praesent odio lacus, molestie sit amet sem in, sollicitudin efficitur tellus. Donec efficitur est eu tellus condimentum fermentum. Donec eu lacinia mi. Pellentesque cursus lobortis tincidunt. Aenean consequat magna a lacinia ornare. Phasellus nulla est, feugiat sit amet turpis at, fermentum pretium dui. Aliquam quis varius eros, ut suscipit purus. Aliquam a tempus risus.\nAliquam hendrerit ipsum magna, quis porta justo scelerisque id. Aliquam erat volutpat. Integer eleifend lorem sollicitudin, porta ante non, sollicitudin lectus. Sed et venenatis arcu, consectetur ullamcorper massa. Phasellus quis mi id leo placerat malesuada. Nunc a pharetra leo. Donec ac diam blandit, auctor arcu nec, venenatis augue. Sed eget mattis risus. Quisque vitae laoreet lacus. Suspendisse potenti. Pellentesque commodo lorem libero, at hendrerit diam molestie lobortis. Cras nec tortor ut magna convallis venenatis rhoncus sed elit. Nam congue quam sit amet est placerat condimentum. Quisque orci ante, cursus non fermentum in, commodo a magna. Duis eleifend vestibulum orci id maximus. Duis fermentum congue dapibus.\nNulla nulla augue, tempus ut lectus nec, dictum faucibus elit. Aliquam ex neque, tincidunt sit amet interdum vitae, ultricies ut dui. Nulla gravida sagittis volutpat. Ut egestas facilisis sapien, vel blandit nisl maximus id. Integer dolor magna, euismod ac aliquam a, tincidunt vitae libero. Phasellus justo felis, cursus id vulputate sit amet, maximus at sem. Nulla nec vestibulum lorem, a scelerisque justo. Cras et ultricies leo, et luctus quam. Curabitur sollicitudin suscipit purus, vel semper turpis feugiat eu. Etiam vehicula et lorem quis porttitor.\nMaecenas semper libero id rutrum auctor. Mauris a mollis turpis, a pretium odio. Nullam elementum porta magna quis placerat. Donec fringilla diam nibh, vitae scelerisque orci hendrerit quis. In finibus consectetur felis. Sed accumsan pretium augue a ornare. Morbi vulputate, lectus vitae feugiat ornare, mauris augue pellentesque diam, at dapibus sapien urna vitae dolor. Suspendisse potenti. Pellentesque ut mi justo. Aliquam finibus ligula quis finibus cursus. Aenean egestas nibh nec finibus volutpat. Cras gravida quis tellus eu venenatis. Pellentesque quis ullamcorper lacus."
