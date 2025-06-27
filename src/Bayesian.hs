{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
--{-# LANGUAGE OverloadedLists #-}

module Bayesian
    ( getRows, showAsRows, normalize, queryNetwork, parseNetwork, mapTableKeys
    , Columns (Columns, ColumnsIdentity)
    , BayesValuation (Table)
    , ProbabilityQuery
    , Probability
    )
where

import           ShenoyShafer
import           Utils
import           Utils                                    (nubWithBy, setMap)
import           ValuationAlgebra

import           Control.Exception                        (assert)
import           Data.Binary                              (Binary)
import           Data.Functor                             (void)
import           Data.Functor.Identity                    (Identity)
import           Data.Map                                 (Map)
import qualified Data.Map                                 as M
import           Data.Set                                 (Set, empty, fromList,
                                                           intersection, member,
                                                           toList, union)
import qualified Data.Set                                 as S
import           Debug.Trace
import           GHC.Generics
import           System.IO                                (openFile, hGetContents', IOMode (ReadMode))
import           Text.Parsec.Char                         (endOfLine)
import           Text.Parsec.Language                     (haskellDef)
import           Text.Parsec.Token                        (GenTokenParser,
                                                           float,
                                                           makeTokenParser)
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec            as P


import           Control.Distributed.Process
import           Control.Distributed.Process.Serializable

{-
In BValColumns, the first parameter is the variable, the second parameter
is a list of the conditions, and the third parameter is the list of probabilities,
where probabilities are ordered as follows:

    var   A   B   Probability

     0    0   0       0.7       P(var == 0 | A == 0 && B == 0)

     0    0   1       0.4       P(var == 0 | A == 0 && B == 1)

     0    0   2       0.3       P(var == 0 | A == 0 && B == 2)

     0    1   0       0.6       P(var == 0 | A == 1 && B == 0)

     .    .   .        .                      .
     .    .   .        .                      .
     .    .   .        .                      .

BValRows stores the equivalent information except as what is essentially as a tuple of each
row of the table instead.

Note that neither form allows the range of values that can be stored in a certain field differ across fields;
i.e. if 'A' can range from 0 to 1, then B must also range from 0 to 1.

BValColumns stores no redundant information, while BValRows stores a heap of redundant information,
but allows accessing this information in a more haskell-like manner.
-}
data BayesValuation a b = Table [Row a b] | Identity deriving (Generic, Binary)

-- Don't be suprised if you need to put (Enum, bounded) on 'b'.
instance Valuation BayesValuation where
    label Identity        = empty
    label (Table [])      = empty
    label (Table (x : _)) = M.keysSet (variables x)

    -- Identity / neutral element must be addressed here, or a plan made to address it in the main typeclass.
    combine Identity x = x
    combine x Identity = x
    combine (Table []) _ = Table []
    combine _ (Table []) = Table []
    combine (Table (x:xs)) (Table (y:ys)) = Table $
            [Row (unionUnsafe (variables a) (variables b)) (probability a * probability b)
                | a <- (x:xs), b <- (y:ys), sharedVariablesAreSameValue numSharedVars a b]

        where
            numSharedVars = fromIntegral . length $ intersection (M.keysSet (variables x)) (M.keysSet (variables y))

    -- todo can upgrade to hashmap.
    project Identity _ = Identity
    project (Table xs) domain = Table $ nubWithBy (\(Row vs _) -> vs) addRows $ map (\(Row vs p) -> Row (projectedDomain vs) p) xs
        where
            projectedDomain = M.filterWithKey (\k _ -> k `elem` domain)
            addRows (Row vs1 p1) (Row vs2 p2) = assert (vs1 == vs2) $ Row vs1 (p1 + p2)

    identity = Identity

instance (Show a, Show b) => Show (BayesValuation a b) where
    show = showAsRows

showAsRows :: (Show a, Show b) => BayesValuation a b -> String
showAsRows (Table xs) = "------ Table ------\n"
                     ++ concatMap (\(Row vs p) -> show vs ++ "   " ++ show p ++ "\n") xs
                     ++ "-------------------\n"
showAsRows Identity = "------ Table ------\n"
                   ++ "Identity"
                   ++ "-------------------\n"


-- An inefficent storage format, but we should get a working implementation first.
data Row a b = Row
    {
        variables   :: Variables a b,
        probability :: Probability
    }
    deriving (Show, Generic, Binary)

type Variable a b = (a, b)
type Variables a b = M.Map a b

-- A supporting data structure, as inputting data in this format is often easier.
data Columns a b = Columns [a] [Probability] | ColumnsIdentity deriving (Show)

getRows :: forall a b. (Enum b, Bounded b, Ord a) => Columns a b -> BayesValuation a b
getRows ColumnsIdentity = Identity
getRows (Columns vars ps) = Table $ zipWith Row (vPermutations vars) ps
    where
        varValues :: [b]
        varValues = [minBound .. maxBound]

        vPermutations :: [a] -> [Variables a b]
        vPermutations xs = map fromListAssertDisjoint $ vPermutations' xs
            where
                vPermutations' :: [a] -> [[Variable a b]]
                vPermutations' [] = [[]]
                vPermutations' (v : vs) = [(v, vVal) : rest | vVal <- varValues, rest <- vPermutations' vs]


normalize :: BayesValuation a b -> BayesValuation a b
normalize Identity = Identity
normalize (Table xs) = Table $ fmap (\(Row vs p) -> Row vs (p / sumOfAllPs)) xs
    where
        sumOfAllPs = sum $ map (\(Row _ p) -> p) xs

type Probability = Double
type Network a b = [BayesValuation a b]
type ProbabilityQuery a b = (M.Map a b, M.Map a b)

-- Returns true iff the two rows should be combined as a part of a combine operation.
-- The rows should be combined if all of their shared variables are the same value.
sharedVariablesAreSameValue :: (Ord a, Ord b) => Integer -> Row a b -> Row a b -> Bool
sharedVariablesAreSameValue numSharedVariables x y =
        fromIntegral (length (intersection (S.fromList $ M.assocs $ variables x) (S.fromList $ M.assocs $ variables y))) == numSharedVariables

conditionalProbability :: (Ord a) => Variables a b -> Variables a b -> (Variables a b -> Probability) -> Probability
conditionalProbability vs givenVs p = p (unionAssertDisjoint vs givenVs) / p givenVs

-- unsafe
findProbability :: (Eq a, Eq b) => Variables a b -> BayesValuation a b -> Probability
findProbability x (Table rows) = (\(Row _ p) -> p) $ findAssertSingleMatch (\(Row vs _) -> vs == x) rows
findProbability _ Identity = error "findProbability: Attempted to read probability from an identity valuation."

queryNetwork :: forall a b. (Serializable a, Serializable b, Ord a, Ord b)
    => [ProbabilityQuery a b]
    -> Network a b
    -> Process [Probability]
queryNetwork qs network' = do
    results <- inference network' queriesForInference
    let f vs = queryToProbability vs results
    pure $ map (\(vs, givenVs) -> conditionalProbability vs givenVs f) qs

    where
        queriesForInference :: [Domain a]
        queriesForInference = map (\(vs, givenVs) -> assert (S.disjoint (M.keysSet vs) (M.keysSet givenVs)) $
                                   union (M.keysSet vs) (M.keysSet givenVs)) qs

{- | Takes a query and returns the resulting probability. Assumes the query is covered by the network. -}
queryToProbability :: (Ord a, Ord b) => Variables a b -> InferredData BayesValuation a b -> Probability
queryToProbability vs results = findProbability vs (normalize $ answerQuery (M.keysSet vs) results)

mapTableKeys :: (Ord b) => (a -> b) -> BayesValuation a c -> BayesValuation b c
mapTableKeys f (Table xs) = Table $ map (\(Row vs p) -> Row (M.mapKeys f vs) p) xs
mapTableKeys _ Identity = Identity

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
        pure (trueP, falseP)
    <?> "tuple inside data"

