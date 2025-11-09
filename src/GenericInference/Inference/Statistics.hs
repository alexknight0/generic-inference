{-# LANGUAGE CPP            #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

-- Warnings caused by CPP not compiling certain code
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module GenericInference.Inference.Statistics (
      Stats (..)
    , empty
    , fromTree
    , fromForest
    , fromLargestNode
    , lift

    , WithStats (..)
    , withStats
    , withNoStats
) where
import qualified Data.List                                  as L
import           Data.Maybe                                 (fromJust,
                                                             fromMaybe)
import qualified Data.Set                                   as S
import qualified GenericInference.Inference.JoinTree        as JF
import qualified GenericInference.Inference.JoinTree.Forest as JF
import qualified GenericInference.Inference.JoinTree.Tree   as JT
import qualified GenericInference.ValuationAlgebra          as V

-- | Data structure containg some statistics about an inference process.
-- The lists are aligned by index (i.e. for use with zipWith).
--
-- __Warning__: This interface is not too developed and is liable to
-- change greatly in future versions.
data Stats = Stats {
      -- | The size of the domain of the node with the
      -- largest domain (we say 'largest node label').
      treeWidths            :: [Int]

      -- | Number of valuations
    , valuations            :: [Int]

      -- | Number of vertices
    , treeVertices          :: [Int]

      -- | The sum of the frame lengths of each variable of the
      -- node with the largest label.
    , sumFrameLengths       :: [V.IntOrInfinity]

      -- The size of the domain obtained by unioning all valuations
    , overarchingDomainSize :: [Int]
} deriving (V.NFData, V.Generic)


empty :: Stats
empty = Stats [] [] [] [] []

includeTree :: (V.Valuation v a) => JT.JoinTree v a -> Stats -> Stats
includeTree t s = s { treeWidths            = s.treeWidths            ++ [treeWidth]
                    , valuations            = s.valuations            ++ [treeValuationCount]
                    , treeVertices          = s.treeVertices          ++ [treeVertices]
                    , sumFrameLengths       = s.sumFrameLengths       ++ [treeSumFrameLengths]
                    , overarchingDomainSize = s.overarchingDomainSize ++ [treeTotalDomainLength]
                   }
    where
        treeWidth             = JT.treeWidth $ t
        treeValuations        = filter (\n -> n.t == JT.Valuation) . JT.vertexList $ t
        treeValuationCount    = length treeValuations
        treeTotalDomainLength = S.size $ S.unions $ map (.d) treeValuations

        treeVertices       = length . JT.vertexList $ t
        treeSumFrameLengths = JT.treeSumFrameLengths t

fromTree :: (V.ValuationFamily v, Show a, Ord a) => JT.JoinTree v a -> Stats
#if !defined(COUNT_OPERATIONS) || !(COUNT_OPERATIONS)
fromTree _ = empty
#else
fromTree t = includeTree t empty
#endif

fromForest :: (V.Valuation v a) => JF.JoinForest v a -> Stats
#if !defined(COUNT_OPERATIONS) || !(COUNT_OPERATIONS)
fromForest _ = empty
#else
fromForest f = foldr includeTree empty $ JF.treesWithQueryNodes f
#endif

fromLargestNode :: V.Valuation v a => Maybe (v a) -> [v a]-> Stats
#if !defined(COUNT_OPERATIONS) || !(COUNT_OPERATIONS)
fromLargestNode _ _                      = empty
#else
fromLargestNode largest valuations = Stats {
        treeWidths            = [fromMaybe 0 $ fmap V.labelSize largest]
      , valuations            = [length valuations]
      , treeVertices          = [0]
      , sumFrameLengths       = [maxFrameLength]
      , overarchingDomainSize = [S.size $ S.unions $ map V.label valuations]
    }

    where
        frameLength var = V.frameLength var valuationThatContainsVar
            where
                -- Guaranteed to exist as variable would not occur unless there
                -- was a valuation in which it occured.
                valuationThatContainsVar = fromJust $ L.find (\v -> S.member var (V.label v)) valuations

        maxFrameLength = case largest of
                                Nothing -> V.Int 0
                                Just l  -> product' . map frameLength . S.toList . V.label $ l

        product' :: [V.IntOrInfinity] -> V.IntOrInfinity
        product' = foldr f (V.Int 1)
            where
                f (V.Int x) (V.Int y) = V.Int $ x * y
                f _         _         = V.Infinity
#endif

append :: Stats -> Stats -> Stats
append s1 s2 = s1 { treeWidths            = s1.treeWidths            ++ s2.treeWidths
                  , valuations            = s1.valuations            ++ s2.valuations
                  , treeVertices          = s1.treeVertices          ++ s2.treeVertices
                  , sumFrameLengths       = s1.sumFrameLengths       ++ s2.sumFrameLengths
                  , overarchingDomainSize = s1.overarchingDomainSize ++ s2.overarchingDomainSize
                }

--------------------------------------------------------------------------------
-- Data wrapper
--------------------------------------------------------------------------------
-- | Data structure allowing easy passing of stats around.
data WithStats a = WithStats { stats :: Stats, c :: a } deriving (V.NFData, V.Generic)

withStats :: Stats -> a -> WithStats a
withStats = WithStats

instance Functor WithStats where
    fmap f s = withStats s.stats (f s.c)

lift :: [WithStats a] -> WithStats [a]
lift stats = foldr f (withStats empty []) stats
    where
        f :: WithStats a -> WithStats [a] -> WithStats [a]
        f s acc = withStats (s.stats `append` acc.stats) (s.c : acc.c)

withNoStats :: a -> WithStats a
withNoStats = withStats empty




