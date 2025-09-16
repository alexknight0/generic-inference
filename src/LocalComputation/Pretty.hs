module LocalComputation.Pretty (
      showTable
    , Table
    , table
) where

import qualified LocalComputation.Utils as U
import           Text.PrettyPrint.Boxes

data Table = Table {
      headings :: [String]
    , rows     :: [[String]]
}

table :: [String] -> [[String]] -> Table
table heading rows' = U.assertP satisfiesInvariants $ Table heading rows'

toBox :: Table -> Box
toBox (Table [] _) = nullBox
toBox t              = hsep 2 left [column, toBox rest]
    where
        column = vsep 1 left [text (head t.headings), vcat left (map (text . head) t.rows)]
        rest = t { headings = tail t.headings
                 , rows    = map tail t.rows
                }

showTable :: Table -> String
showTable = render . toBox

thesisExample :: Table
thesisExample = undefined

--------------------------------------------------------------------------------
-- Invariants
--------------------------------------------------------------------------------
satisfiesInvariants :: Table -> Bool
satisfiesInvariants t
    | length t.rows /= 0 = all (\row -> length row == length t.headings) t.rows
    | otherwise = True



