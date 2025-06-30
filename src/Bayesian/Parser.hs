module Bayesian.Parser
    (
        parseNetwork, parseNetwork'
    )
where

import           Bayesian
import           SemiringValuationAlgebra

import           Data.Functor.Identity         (Identity)
import           System.IO                     (IOMode (ReadMode),
                                                hGetContents', openFile)
import           Text.Parsec.Char              (endOfLine)
import           Text.Parsec.Language          (haskellDef)
import           Text.Parsec.Token             (GenTokenParser, float,
                                                makeTokenParser)
import           Text.ParserCombinators.Parsec


parseNetwork' :: String -> String -> Either ParseError (Network String Bool)
parseNetwork' xs filename = parse network filename xs

parseNetwork :: FilePath -> IO (Either ParseError (Network String Bool))
parseNetwork filename = do
    handle <- openFile filename ReadMode
    contents <- hGetContents' handle
    pure $ parseNetwork' contents filename

network :: GenParser Char st (Network String Bool)
network = do
    emptyHeader
    _ <- many node
    potentials <- many potential
    _ <- many spacesAndNewLine
    eof

    pure potentials

spaces' :: GenParser Char st ()
spaces' = skipMany (oneOf " \t")

spacesAndNewLine :: GenParser Char st ()
spacesAndNewLine = do
    spaces'
    _ <- endOfLine
    pure ()

notNewLineNorNoneOf :: String -> GenParser Char st Char
notNewLineNorNoneOf xs = do
    notFollowedBy endOfLine
    noneOf xs

-- Used to tell the parser whose definition of 'float' we are using when
-- we say we want to parse a float.
lexer :: GenTokenParser String u Identity
lexer = makeTokenParser haskellDef


emptyHeader :: GenParser Char st ()
emptyHeader = do
        skipMany spacesAndNewLine
        _ <- string "net"
        _ <- spacesAndNewLine
        _ <- char '{'
        _ <- spacesAndNewLine
        _ <- char '}'
        _ <- spacesAndNewLine
        pure ()
    <?> "empty header"


{- | Parses a node. For example;

\n{\n  states = ( \"yes\" \"no\" );\n}\n
-}
node :: GenParser Char st ()
node = do
        skipMany spacesAndNewLine

        _ <- spaces' >> string "node " >> many1 (notNewLineNorNoneOf " ") >> spacesAndNewLine

        _ <- char '{' >> spacesAndNewLine

        _ <- spaces' >> string "states" >> spaces' >> char '=' >> spaces' >> char '('
        _ <- spaces' >> many1 (notNewLineNorNoneOf " ") >> spaces' >> many1 (notNewLineNorNoneOf " ")
        _ <- spaces' >> char ')' >> spaces' >> char ';' >> spacesAndNewLine

        _ <- char '}' >> spacesAndNewLine
        pure ()
    <?> "node"

-- todo may not have conditional vars. i.e. terminators and initials.
potential :: GenParser Char st (BayesValuation String Bool)
potential = do
        skipMany spacesAndNewLine

        _ <- string "potential" >> spaces' >> char '(' >> spaces'
        conditionedVar <- many1 (notNewLineNorNoneOf " |)")
        _ <- spaces' >> optionMaybe (char '|') >> spaces'
        conditionalVars <- many $ do
            _ <- spaces'
            conditionalVar <- many1 (notNewLineNorNoneOf " |)")
            _ <- spaces'
            pure conditionalVar
        _ <- char ')' >> spacesAndNewLine

        _ <- char '{' >> spacesAndNewLine
        _ <- spaces' >> string "data" >> spaces' >> char '=' >> spaces'
        probabilities <- potentialData
        _ <- spaces' >> char ';' >> spacesAndNewLine

        _ <- char '}' >> spacesAndNewLine
        pure (getRows $ Columns (conditionedVar : conditionalVars) probabilities)
    <?> "potential"

-- We don't worry about verifying the file is in the correct format here,
-- we simply throw away all brackets and just read the numbers sequentially.
potentialData :: GenParser Char st [Probability]
potentialData = do
        probabilities <- many $ potentialDataEntry

        -- Convert to list that iterates probabilities like a truth table that starts at FFF.
        pure (reverse $ map fst probabilities ++ map snd probabilities)
    <?> "data"

potentialDataEntry :: GenParser Char st (Probability, Probability)
potentialDataEntry = do
        _ <- many $ oneOf "( "
        trueP <- float lexer
        _ <- spaces'
        falseP <- float lexer
        _ <- many $ oneOf ") "
        pure (P trueP, P falseP)
    <?> "tuple inside data"
