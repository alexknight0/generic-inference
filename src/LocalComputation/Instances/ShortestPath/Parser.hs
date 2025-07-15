{- | Parser to parse .gr files into a graph suitable for use in the library.

For information on the .gr file format see: https://www.diag.uniroma1.it/challenge9/format.shtml#graph
-}
module LocalComputation.Instances.ShortestPath.Parser
    ( graph
    , Graph
    )
where

import           Control.Applicative           ((<|>))
import           Control.Monad                 (void)
import           Data.Either                   (isRight)
import           Data.Functor.Identity         (Identity)
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           Numeric.Natural               (Natural)
import qualified Text.Parsec.Char              as P (endOfLine)
import qualified Text.Parsec.Language          as P (haskellDef)
import qualified Text.Parsec.Token             as P
import qualified Text.ParserCombinators.Parsec as P

type Graph a b = M.Map a [(a, b)]

graph :: P.GenParser Char st (Graph Natural Integer)
graph = do
    P.skipMany $ P.try comment
    (numNodes, numArcs) <- problem
    arcs <- fmap getRight $ P.many $ (Left <$> P.try comment) <|> (Right <$> P.try arc)

    -- Create graph
    let g = foldr f M.empty arcs
        actualNumNodes = fromIntegral $ length $ S.union (M.keysSet g)
                                                         (S.fromList $ concat $ map (map fst) (M.elems g))
        actualNumArcs = fromIntegral $ length arcs

    -- Check graph matches problem statement at top of file
    parseAssert (actualNumArcs == numArcs) $
        "Found " ++ show actualNumArcs ++ " arcs, from problem statement expected " ++ show numArcs ++ " arcs."
    parseAssert (actualNumNodes == numNodes) $
        "Found " ++ show actualNumNodes ++ " nodes, from problem statement expected " ++ show numNodes ++ " nodes."

    pure g

    where
        f (arcTail, arcHead, weight) acc = M.insertWith (++) arcTail [(arcHead, weight)] acc

        getRight :: [Either a b] -> [b]
        getRight xs = map fromRight $ filter isRight xs

        fromRight (Right x) = x
        fromRight _         = error "Call to fromRight on a Left element"

spaces :: P.GenParser Char st ()
spaces = P.skipMany (P.oneOf " \t")

parseAssert :: Bool -> String -> P.GenParser Char st ()
parseAssert b msg
    | b = pure ()
    | otherwise = fail msg

comment :: P.GenParser Char st ()
comment = do
    spaces
    void $ P.char 'c'
    spaces
    void $ P.endOfLine

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
    spaces
    void $ P.endOfLine

    pure (fromIntegral nodes, fromIntegral arcs)

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
    spaces
    void $ P.endOfLine

    pure (fromIntegral arcTail, fromIntegral arcHead, weight)

-- Used to tell the parser whose definition of 'integer' we are using when
-- we say we want to parse a 'integer'.
lexer :: P.GenTokenParser String u Identity
lexer = P.makeTokenParser P.haskellDef

