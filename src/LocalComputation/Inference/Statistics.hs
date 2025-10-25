{-# LANGUAGE CPP            #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module LocalComputation.Inference.Statistics (
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
import qualified LocalComputation.Inference.JoinTree        as JF
import qualified LocalComputation.Inference.JoinTree.Forest as JF
import qualified LocalComputation.Inference.JoinTree.Tree   as JT
import qualified LocalComputation.ValuationAlgebra          as V

data Stats = Stats {
      treeWidths            :: [Int]
    , treeValuations        :: [Int]
    , treeVertices          :: [Int]
    -- Take the tree node that has the largest label, then
    -- find the variable that has the largest frame length.
    -- The resulting frame length is the `treeMaxFrameLength`.
    , treeSumFrameLengths   :: [V.IntOrInfinity]
    , treeTotalDomainLength :: [Int]
} deriving (V.NFData, V.Generic)


empty :: Stats
empty = Stats [] [] [] [] []

includeTree :: (V.Valuation v a) => JT.JoinTree v a -> Stats -> Stats
includeTree t s = s { treeWidths            = s.treeWidths            ++ [treeWidth]
                    , treeValuations        = s.treeValuations        ++ [treeValuationCount]
                    , treeVertices          = s.treeVertices          ++ [treeVertices]
                    , treeSumFrameLengths    = s.treeSumFrameLengths    ++ [treeSumFrameLengths]
                    , treeTotalDomainLength = s.treeTotalDomainLength ++ [treeTotalDomainLength]
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
-- #if !defined(COUNT_OPERATIONS) || !(COUNT_OPERATIONS)
-- fromLargestNode _ _                      = empty
-- #else
fromLargestNode largest valuations = Stats {
        treeWidths         = [fromMaybe 0 $ fmap V.labelSize largest]
      , treeValuations     = [length valuations]
      , treeVertices       = [0]
      , treeSumFrameLengths = [maxFrameLength]
      , treeTotalDomainLength = [S.size $ S.unions $ map V.label valuations]
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
-- #endif

append :: Stats -> Stats -> Stats
append s1 s2 = s1 { treeWidths            = s1.treeWidths            ++ s2.treeWidths
                  , treeValuations        = s1.treeValuations        ++ s2.treeValuations
                  , treeVertices          = s1.treeVertices          ++ s2.treeVertices
                  , treeSumFrameLengths   = s1.treeSumFrameLengths   ++ s2.treeSumFrameLengths
                  , treeTotalDomainLength = s1.treeTotalDomainLength ++ s2.treeTotalDomainLength
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




