{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

-- TODO: Add to future work: fully animating the diagram (showing changing animations and everything) through using a GIF.
module LocalComputation.Inference.JoinTree.Diagram (
      draw
    , DrawSettings (beforeInference, afterInference)
    , def
) where

import qualified Data.Colour                                as C
import           Diagrams.Backend.SVG                       (renderSVG)
import           Diagrams.Backend.SVG.CmdLine
import           Diagrams.Prelude                           hiding (def)
import qualified Graphics.SVGFonts                          as SF

import           Control.Exception                          (assert)
import qualified Data.List                                  as L
import qualified Data.List.Extra                            as L (splitOn)
import qualified Graphics.SVGFonts.ReadFont                 as SF
import qualified LocalComputation.Inference.JoinTree        as JT
import qualified LocalComputation.Inference.JoinTree.Forest as JT
import qualified LocalComputation.ValuationAlgebra          as V
import           System.Directory.Extra                     (createDirectoryIfMissing)
import           System.FilePath                            (takeDirectory)

data DrawSettings = DrawSettings {
      beforeInference :: Maybe FilePath
    , afterInference  :: Maybe FilePath
}

def :: DrawSettings
def = DrawSettings {
      beforeInference = Nothing
    , afterInference = Nothing
}

data DiagramWithBorder a = DiagramWithBorder {
    diagram     :: Diagram a,
    borderWidth :: Double
}

-- TODO: A different data structure for the graph could be better here. Additionally, we might want
-- to unite all graphs under one data structure to prevent confusion.

-- TODO: Add more options to draw (such as size)

-- Draws a tree from the given graph, outputting the drawing into a file of the given name. Has the same assumptions as `tree`.
draw :: (V.Valuation v, Show (v a), Ord a, Show a)
    => FilePath -> JT.JoinForest (v a) -> IO ()
draw name g = do
    -- Create directory for file if necessary
    createDirectoryIfMissing True (takeDirectory name)

    -- Load the chosen font
    chosenFont <- SF.bit

    -- Create the diagram
    let diagram = cat (r2 (2, 3)) [legend chosenFont # scaleProportional 0.2 t, t]
        t = tree chosenFont g

    -- Render the diagram
    renderSVG name (dims2D 1400 1400) (diagram # framePadding 0.05)

-- | Produces a diagram of a tree out of a given graph.
--
-- Assumes the given graph has a tree like structure, as specified in the description of `baseJoinTree`,
-- and that the node with the highest `id` is the root.
tree :: (V.Valuation v, Show (v a), Ord a, Show a)
    => SF.PreparedFont Double -> JT.JoinForest (v a) -> Diagram B
tree chosenFont g = assert (length rootOutgoingEdges == 0) $
                           tree' chosenFont root g
    where
        root = g.root

        rootOutgoingEdges = JT.unsafeOutgoingEdges root.id g

-- | Produces a diagram of a tree by following all **incoming** edges from a given node in a graph. Assumes the given node is in the tree.
tree' :: (V.Valuation v, Show (v a), Ord a, Show a)
    => SF.PreparedFont Double -> JT.Node (v a) -> JT.JoinForest (v a) -> Diagram B
tree' chosenFont node g = vsep vgap [root.diagram, parents] # applyAll arrows
    where
        root = treeNode chosenFont node

        -- Horribly inefficent, constantly recalculated
        incoming = JT.unsafeIncomingEdges node.id g
        parents = centerX . hsep hgap . map alignT . map (\n -> tree' chosenFont n g) $ incoming

        arrows = map (\parent -> connectOutside' arrowOpts parent.id node.id # lwL root.borderWidth) incoming
        arrowOpts = with & headLength .~ local (root.borderWidth * 8)
                         & headGap    .~ local (root.borderWidth * 6)
                         & tailGap    .~ local (root.borderWidth * 6)
        vgap = 0.5  * height root.diagram
        hgap = 0.25 * width root.diagram

-- | Produces a diagram representing a node in a tree.
treeNode :: (V.Valuation v, Show (v a), Ord a, Show a)
    => SF.PreparedFont Double -> JT.Node (v a) -> DiagramWithBorder B
treeNode chosenFont node = DiagramWithBorder (full # named node.id) contents.borderWidth
    where
        full = vsep 0 [header, body]

        header           = headerText <> headerBackground
        headerBackground = rect (width body) (height headerText + 2 * seperationSpace) # fc black
        headerText       = textWithNewlines chosenFont white ("NODE " ++ show node.id)       # scale 3

        body     = contents.diagram # bgA (nodeColour node.t)
        contents = withBorder $ vsep seperationSpace [titleText "DOMAIN",    domain, seperator,
                                                      titleText "VALUATION", valuation]
        domain         = textWithNewlines chosenFont black (V.showDomain $ V.label node.v)  # scale 3
        valuation      = textWithNewlines chosenFont black (show node.v)                    # scale 1
        titleText s    = textWithNewlines chosenFont black s                                # scale 1 # opacity 0.7

        seperator = line (maximum [width domain, width valuation]) # opacity 0.5

        seperationSpace = height domain / 4

-- | Returns the colour for a given node type.
nodeColour :: JT.NodeType -> AlphaColour Double
nodeColour t = case t of
                  JT.Valuation  -> darkblue              `C.withOpacity` 0.15
                  JT.Query      -> skyblue               `C.withOpacity` 0.60
                  JT.Union      -> orange                `C.withOpacity` 0.15
                  JT.Projection -> darken 0.5 darkorange `C.withOpacity` 0.15

-- | A legend identifying the colours of all possible node types.
legend :: SF.PreparedFont Double -> Diagram B
legend chosenFont = full <> border
    where
        full = vsep 0 (header : L.intersperse (line w) entries) # centerXY
        border = rect (width full) (height full) # lw ultraThin

        header = headerText # fixedBgA w h (black `withOpacity` 1)
        headerText = textWithNewlines chosenFont white "LEGEND"

        nodeTypes :: [JT.NodeType]
        nodeTypes = [minBound .. maxBound]

        entriesText = map     (\t -> textWithNewlines chosenFont black (show t ++ " Node")) nodeTypes
        entries     = zipWith (\t e -> fixedBgA w h (nodeColour t) e)                       nodeTypes entriesText

        w = 1.5 * maximum (map width  (headerText : entriesText))
        h = 1.5 * maximum (map height (headerText : entriesText))

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Adds a border to the given element. Used with `treeNode`.
withBorder :: Diagram B -> DiagramWithBorder B
withBorder x = DiagramWithBorder ((textWithPadding <> rectangle) # withEnvelope rectangle)
                              borderWidth
    where
        paddingSize = min (width x) (height x) * 0.1
        textWithPadding = centerXY $ strutY (paddingSize / 2) === (strutX paddingSize ||| x # centerXY ||| strutX paddingSize) === strutY paddingSize

        rectangle = rect (width textWithPadding) (height textWithPadding) # borderStyles
        borderWidth = 0.2 * paddingSize
        borderStyles = lwL (borderWidth / 2) # dashingL [borderWidth * 2, borderWidth * 3] 0 # opacity 0.8

-- | A line of width 'w'.
line :: Double -> Diagram B
line w = arrowV' arrowOpts (r2 (w, 0)) # lw ultraThin # centerXY
    where
        arrowOpts = with & arrowHead .~ noHead

-- | Variant of `textWithEnvelope` that handles newlines by putting the text on a new line.
textWithNewlines :: SF.PreparedFont Double -> Colour Double -> String -> Diagram B
textWithNewlines chosenFont colour s = body # centerXY
    where
        body = foldr ((===) . textWithEnvelope chosenFont colour) mempty $ L.splitOn "\n" s

-- | Text that has an envelope representing its size.
textWithEnvelope :: SF.PreparedFont Double -> Colour Double -> String -> Diagram B
textWithEnvelope chosenFont colour s = SF.svgText (SF.TextOpts chosenFont SF.HADV False) s # SF.set_envelope # lw none # fc colour


-- | Alternative to `pad` that is not affected by whether the diagram is centred.
-- Pads all sides by the same amount, proportional to the ratio multiplied by the
-- minimum of the weight and height.
framePadding :: Double -> Diagram B -> Diagram B
framePadding ratio x = x # frame (ratio * min (width x) (height x))

-- | Scales a diagram so that it is proportional to another diagram.
--
-- Might want to look into `requiredScaling`.
scaleProportional :: Double -> Diagram B -> Diagram B -> Diagram B
scaleProportional ratio reference x = x # scale result
    where
        newWidth  = ratio * width  reference
        newHeight = ratio * height reference

        requiredWidthScaling  = newWidth  / width  x
        requiredHeightScaling = newHeight / height x

        result = min requiredWidthScaling requiredHeightScaling

-- | Variant of `bg` that allows alpha values.
--
-- __Warning__: Will center local origin of diagram, and may not behave like `bg` does.
bgA :: AlphaColour Double -> Diagram B -> Diagram B
bgA colour x = x # centerXY
                    <> rect (width x) (height x) # centerXY # lw 0 # fcA colour

-- | Creates a background of a fixed size.
--
-- __Warning__: Will center local origin of diagram, and may not behave like `bg` does.
fixedBgA :: Double -> Double -> AlphaColour Double -> Diagram B -> Diagram B
fixedBgA w h colour x = x # centerXY
                    <> rect w h # centerXY # lw 0 # fcA colour
