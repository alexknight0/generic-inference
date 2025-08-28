{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

-- TODO: Add to future work: fully animating the diagram (showing changing animations and everything) through using a GIF.
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
import qualified Graphics.SVGFonts.ReadFont          as SF
import qualified LocalComputation.Inference.JoinTree as JT
import qualified LocalComputation.ValuationAlgebra   as V

-- TODO: A different data structure could be better here. Additionally, we might want
-- to unite all graphs under one data structure to prevent confusion.

draw :: (V.Valuation v, Show (v a b), Ord a, Ord b, Show b, Show a)
    => FilePath -> G.Graph (JT.Node (v a b)) -> IO ()
draw name g = tree g >>= renderSVG name (dims2D 1400 1400)

-- | Assumes a tree like structure, and that the node with the highest `id` is the root.
tree :: (V.Valuation v, Show (v a b), Ord a, Ord b, Show b, Show a)
    => G.Graph (JT.Node (v a b)) -> IO (Diagram B)
tree g = do
    chosenFont <- SF.bit
    assert (length rootOutgoingEdges == 0) (pure ())
    pure $ tree' chosenFont root g # centerXY # pad 1.05
    where
        root = L.maximumBy (\x y -> x.id `compare` y.id) $ G.vertexList g

        rootOutgoingEdges = snd . fromJust . L.find (\(x, _) -> x.id == root.id) . G.adjacencyList $ g

data DiagramWithBorder a = DiagramWithBorder {
    diagram     :: Diagram a,
    borderWidth :: Double
}

withBorder :: Diagram B -> DiagramWithBorder B
withBorder x = DiagramWithBorder ((textWithPadding <> rectangle) # withEnvelope rectangle)
                              borderWidth
    where
        paddingSize = min (width x) (height x) * 0.1
        textWithPadding = centerXY $ strutY (paddingSize / 2) === (strutX paddingSize ||| x # centerXY ||| strutX paddingSize) === strutY paddingSize

        rectangle = rect (width textWithPadding) (height textWithPadding) # borderStyles
        borderWidth = 0.2 * paddingSize
        borderStyles = lwL (borderWidth / 2) # dashingL [borderWidth * 2, borderWidth * 3] 0 # opacity 0.8


textWithNewlines :: (SF.PreparedFont Double) -> Colour Double -> String -> Diagram B
textWithNewlines chosenFont colour s = body # centerXY
    where
        body = foldr ((===) . textWithEnvelope chosenFont colour) mempty $ L.splitOn "\n" s

textWithEnvelope :: (SF.PreparedFont Double) -> Colour Double -> String -> Diagram B
textWithEnvelope chosenFont colour s = SF.svgText (SF.TextOpts chosenFont SF.HADV False) s # SF.set_envelope # lw none # fc colour

treeNode :: (V.Valuation v, Show (v a b), Ord a, Ord b, Show b, Show a)
    => (SF.PreparedFont Double) -> JT.Node (v a b) -> DiagramWithBorder B
treeNode chosenFont node = DiagramWithBorder (full # named node.id) body.borderWidth
    where
        full = vsep 0 [header, body.diagram]

        header           = headerText <> headerBackground
        headerBackground = rect (width body.diagram) (height headerText + 2 * seperationSpace) # fc black
        headerText       = textWithNewlines chosenFont white ("NODE " ++ show node.id)       # scale 3

        body = withBorder $ vsep seperationSpace [titleText "DOMAIN",    domain, seperator,
                                                  titleText "VALUATION", valuation]
        domain         = textWithNewlines chosenFont black (V.showDomain $ V.label node.v)  # scale 3
        valuation      = textWithNewlines chosenFont black (show node.v)                    # scale 1
        titleText s    = textWithNewlines chosenFont black s                                # scale 1 # opacity 0.7

        seperator = line (maximum [width domain, width valuation]) # opacity 0.5

        seperationSpace = height domain / 4



line :: Double -> Diagram B
line w = arrowV' arrowOpts (r2 (w, 0)) # lw ultraThin # centerXY
    where
        arrowOpts = with & arrowHead .~ noHead

-- | Assumes the given node is in the tree.
tree' :: (V.Valuation v, Show (v a b), Ord a, Ord b, Show b, Show a)
    => (SF.PreparedFont Double) -> JT.Node (v a b) -> G.Graph (JT.Node (v a b)) -> Diagram B
tree' chosenFont node g = vsep vgap [root.diagram, parents] # applyAll arrows
    where
        -- root = text (show node.id) <> circle 1 # named node.id
        root = treeNode chosenFont node

        -- Horribly inefficent, constantly recalculated
        incoming = snd . fromJust . L.find (\(x, _) -> x.id == node.id) . G.adjacencyList . G.transpose $ g
        parents = centerX . hsep hgap . map alignT . map (\n -> tree' chosenFont n g) $ incoming

        arrows = map (\parent -> connectOutside' arrowOpts parent.id node.id # lwL (root.borderWidth / 1.5)) incoming
        arrowOpts = with & headLength .~ local (root.borderWidth * 4.5)
                         & headGap    .~ local (root.borderWidth * 6)
                         & tailGap    .~ local (root.borderWidth * 6)
        vgap = 0.5  * height root.diagram
        hgap = 0.25 * width root.diagram

