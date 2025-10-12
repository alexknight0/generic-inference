{-# LANGUAGE CPP            #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module LocalComputation.Inference.Statistics (
      Stats (..)
    , empty
    , fromTree
    , fromForest
    , fromLargestNode
    , fusionComplexity
    , binaryShenoyComplexity
    , lift

    , WithStats (..)
    , withStats
    , withNoStats
) where
import           Data.Maybe                                 (fromMaybe)
import qualified LocalComputation.Inference.JoinTree        as JF
import qualified LocalComputation.Inference.JoinTree.Forest as JF
import qualified LocalComputation.Inference.JoinTree.Tree   as JT
import qualified LocalComputation.ValuationAlgebra          as V

data Stats = Stats {
      treeWidths         :: [Int]
    , treeValuations     :: [Int]
    , treeVertices       :: [Int]
    -- Take the tree node that has the largest label, then
    -- find the variable that has the largest frame length.
    -- The resulting frame length is the `treeMaxFrameLength`.
    , treeMaxFrameLength :: [V.IntOrInfinity]
} deriving (V.NFData, V.Generic)


empty :: Stats
empty = Stats [] [] [] []

includeTree :: (V.Valuation v a) => JT.JoinTree (v a) -> Stats -> Stats
includeTree t s = s { treeWidths         = s.treeWidths         ++ [treeWidth]
                    , treeValuations     = s.treeValuations     ++ [treeValuations]
                    , treeVertices       = s.treeVertices       ++ [treeVertices]
                    , treeMaxFrameLength = s.treeMaxFrameLength ++ [treeMaxFrameLength]
                   }
    where
        treeWidth          = JT.treeWidth $ t
        treeValuations     = length . filter (\n -> n.t == JT.Valuation) . JT.vertexList $ t
        treeVertices       = length . JT.vertexList $ t
        treeMaxFrameLength = JT.treeMaxFrameLength t

fromTree :: (V.ValuationFamily v, Show a, Ord a) => JT.JoinTree (v a) -> Stats
#if !defined(COUNT_OPERATIONS) || !(COUNT_OPERATIONS)
fromTree _ = empty
#else
fromTree t = includeTree t empty
#endif

fromForest :: (V.Valuation v a) => JF.JoinForest (v a) -> Stats
#if !defined(COUNT_OPERATIONS) || !(COUNT_OPERATIONS)
fromForest _ = empty
#else
fromForest f = foldr includeTree empty $ JF.treesWithQueryNodes f
#endif

fromLargestNode :: V.Valuation v a => Maybe (v a) -> Int -> Stats
-- #if !defined(COUNT_OPERATIONS) || !(COUNT_OPERATIONS)
-- fromLargestNode _ _                      = empty
-- #else
fromLargestNode largest treeValuations = Stats {
        treeWidths         = [fromMaybe 0 $ fmap V.labelSize largest]
      , treeValuations     = [treeValuations]
      , treeVertices       = []
      , treeMaxFrameLength = [fromMaybe (V.Int 0) $ fmap V.maxFrameLength largest]
    }
-- #endif

append :: Stats -> Stats -> Stats
append s1 s2 = s1 { treeWidths         = s1.treeWidths         ++ s2.treeWidths
                  , treeValuations     = s1.treeValuations     ++ s2.treeValuations
                  , treeVertices       = s1.treeVertices       ++ s2.treeVertices
                  , treeMaxFrameLength = s1.treeMaxFrameLength ++ s2.treeMaxFrameLength
                }

--------------------------------------------------------------------------------
-- Complexity Calculation
--------------------------------------------------------------------------------
fusionComplexity :: Stats -> Int
fusionComplexity s = sum $ zipWith f s.treeValuations s.treeWidths
    where
        f m w = m * square (w + 1)

        square x = x * x

-- | Interprets infinity as 0, as we can't properly calculate a value for infinity.
binaryShenoyComplexity :: Stats -> Int
binaryShenoyComplexity stats = sum $ zipWith3 f stats.treeVertices stats.treeMaxFrameLength stats.treeWidths
    where
        f _ V.Infinity _ = 0
        f v (V.Int d)  s = v * d ^ s

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




