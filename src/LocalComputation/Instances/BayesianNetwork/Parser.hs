module LocalComputation.Instances.BayesianNetwork.Parser
    ( network
    )
where

import           LocalComputation.Instances.BayesianNetwork
import qualified LocalComputation.ValuationAlgebra.Semiring as S

import           Control.Exception                          (assert)
import           Data.Functor.Identity                      (Identity)
import qualified Data.List                                  as L
import qualified Data.Map                                   as M
import qualified LocalComputation.Utils                     as M (fromListA)
import           Numeric.Natural                            (Natural)
import           Text.Parsec.Char                           (endOfLine)
import           Text.Parsec.Language                       (haskellDef)
import           Text.Parsec.Token                          (GenTokenParser,
                                                             float,
                                                             makeTokenParser)
import           Text.ParserCombinators.Parsec


data NodeInfo = NodeInfo { name :: String, states :: [String] }

-- | Built to parse a network from a '.net' file. The format specified seems it may be close to
-- the second revision of the net language, but as no documentation could be found, this is uncertain.
-- Notably does not allow potentials that have multiple conditioned variables.
--
-- For information about the net language, see:
-- <https://download.hugin.com/webdocs/manuals/8.9/htmlhelp/pages/Tutorials/CaseAndData/NetLanguage.html>
network :: GenParser Char st (Network String String)
network = do

    s emptyHeader

    nodeInfo   <- many $ s $ node
    potentials <- many $ s $ potential (createLookup nodeInfo)

    eof

    pure potentials

    where
        createLookup :: [NodeInfo] -> M.Map String [String]
        createLookup info = M.fromListA $ map (\n -> (n.name, n.states)) info

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

-- | Used for lexemes that ignores spaces
s :: GenParser Char st a -> GenParser Char st a
s p = spaces *> p <* spaces

emptyHeader :: GenParser Char st ()
emptyHeader = s (string "net") >> s (char '{') >> s (char '}') >> pure ()
    <?> "empty header"


identifier :: GenParser Char st String
identifier = do
        firstChar <- letter <|> char '_'
        rest <- many (alphaNum <|> char '_')
        pure (firstChar : rest)

escapedString :: GenParser Char st String
escapedString = char '"' *> many notNewLineNorQuote <* char '"'
        where
                notNewLineNorQuote = notNewLineNorNoneOf "\""

foobar :: ()
foobar = undefined
{-

>>> parse (many $ notNewLineNorNoneOf "\"") "foo" $ "sff\"s\nfdfdf"
Right "sff"

-}

{-

>>> parse network "foo" $ "net \n{\n \n\nnode GOAL_2 \nstates = ( \"false\" \"true\" );"
Left "foo" (line 5, column 1):
unexpected "n"
expecting space, white space or "}"

-}

{- | Parses a node. For example;

\n{\n  states = ( \"yes\" \"no\" );\n}\n
-}
node :: GenParser Char st NodeInfo
node = do
        name <- s (string "node") *> s (identifier)

        _ <- s (char '{') >> s (string "states") >> s (char '=') >> s (char '(')
        states <- many1 (s (escapedString))
        _ <- s (char ')') >> s (char ';') >> s (char '}')

        pure $ NodeInfo name states
    <?> "node"

-- todo may not have conditional vars. i.e. terminators and initials.
potential :: M.Map String [String] -> GenParser Char st (Valuation String String)
potential states = do
        conditioned <- s (string "potential") >> s (char '(') >> identifier

        conditional <- do
                pipe <- s (optionMaybe (char ('|')))
                vars <- case pipe of
                                Just _  -> many1 (s identifier)
                                Nothing -> pure []
                _    <- s (char ')')
                pure vars

        _ <- s (char '{') >> s (string "data") >> s (char '=')
        probabilities <- many floatFromData
        _ <- s (char ';') >> s (char '}')

        let vars = conditional ++ [conditioned]
            varStates = map (\v -> (v, (M.!) states v)) vars

        pure $ S.getRows varStates probabilities
    <?> "potential"

floatFromData :: GenParser Char st Probability
floatFromData = fmap P (discardJunk (float lexer))
    <?> "float inside data field"
    where
        discardJunk :: GenParser Char st a -> GenParser Char st a
        discardJunk p = many junk *> p <* many junk

        junk = oneOf "() "
