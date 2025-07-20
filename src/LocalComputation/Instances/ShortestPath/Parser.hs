{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}

{- | Parser to parse .gr files into a graph suitable for use in the library.

For information on the .gr file format see: https://www.diag.uniroma1.it/challenge9/format.shtml#graph
-}
module LocalComputation.Instances.ShortestPath.Parser
    ( graph
    , InvalidGraphFile
    , mapParseResult
    , fromValid
    )
where

import qualified LocalComputation.Graph        as G

import           Control.Monad                 (void)
import           Data.Either                   (isRight)
import           Data.Functor.Identity         (Identity)
import           Data.List                     (genericLength)
import           Data.Tuple.Extra              (uncurry3)
import           LocalComputation.Utils
import           Numeric.Natural               (Natural)
import qualified Text.Parsec.Char              as P (endOfLine)
import qualified Text.Parsec.Language          as P (haskellDef)
import qualified Text.Parsec.Token             as P
import qualified Text.ParserCombinators.Parsec as P
import           Text.ParserCombinators.Parsec ((<?>))

data InvalidGraphFile =
      NumNodesMismatch { problemDeclaration :: Natural, numRead :: Natural }
    | NumArcsMismatch  { problemDeclaration :: Natural, numRead :: Natural } deriving Show

-- | An unsafe convenience function for getting the result of a parse.
fromValid :: IO (Either P.ParseError (Either InvalidGraphFile a)) -> IO a
fromValid = fmap (fromRight . fromRight)

graph :: P.GenParser Char st (Either InvalidGraphFile (G.Graph Natural Integer))
graph = do
    P.skipMany $ P.choice [P.try comment, P.try blankLine]
    (numNodes, numArcs) <- problem
    arcs <- fmap getRight $ P.many $ P.choice [Right <$> P.try arc, Left <$> P.try comment, Left <$> P.try blankLine]
    void $ P.manyTill P.space (P.try P.eof)

    let g = G.fromList (map (uncurry3 G.Edge) arcs)
        gNumNodes = fromIntegral $ length $ G.nodes g
        gNumArcs = genericLength arcs

    case () of
        _ | numNodes /= gNumNodes -> pure $ Left $ NumNodesMismatch numNodes gNumNodes
          | numArcs /= gNumArcs   -> pure $ Left $ NumArcsMismatch numArcs gNumArcs
          | otherwise             -> pure $ Right g

    where
        getRight :: [Either a b] -> [b]
        getRight xs = map fromRight $ filter isRight xs

spaces :: P.GenParser Char st ()
spaces = P.skipMany (P.oneOf " \t")

blankLine :: P.GenParser Char st ()
blankLine = do
        spaces
        void $ P.endOfLine
    <?> "blank line"

comment :: P.GenParser Char st ()
comment = do
        spaces
        void $ P.char 'c'
        -- Eat as many non-newline chars as possible
        void $ P.manyTill P.anyChar (P.try P.endOfLine)
    <?> "comment"

problem :: P.GenParser Char st (Natural, Natural)
problem = do
        spaces
        void $ P.char 'p'
        spaces
        void $ P.char 's'
        void $ P.char 'p'
        spaces
        nodes <- P.natural lexer
        spaces
        arcs <- P.natural lexer
        -- The above natural lexer may consume the newline.

        pure (fromIntegral nodes, fromIntegral arcs)
    <?> "problem"


arc :: P.GenParser Char st (Natural, Natural, Integer)
arc = do
        spaces
        void $ P.char 'a'
        spaces
        arcTail <- P.natural lexer
        spaces
        arcHead <- P.natural lexer
        spaces
        weight <- P.integer lexer
        -- The above natural lexer may consume the newline.

        pure (fromIntegral arcTail, fromIntegral arcHead, weight)
    <?> "arc"

-- Used to tell the parser whose definition of 'integer' we are using when
-- we say we want to parse a 'integer'.
lexer :: P.GenTokenParser String u Identity
lexer = P.makeTokenParser P.haskellDef

mapParseResult :: (a -> b)
               -> Either P.ParseError (Either InvalidGraphFile (G.Graph Natural a))
               -> Either P.ParseError (Either InvalidGraphFile (G.Graph Natural b))
mapParseResult f = fmap (fmap (G.mapGraph f))

