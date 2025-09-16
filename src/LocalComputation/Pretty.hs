module LocalComputation.Pretty (
      showTable
    , Table
    , unsafeTable
) where

import qualified LocalComputation.Utils as U
import           Text.PrettyPrint.Boxes hiding (rows)

data Table = Table {
      headings :: [String]
    , rows     :: [[String]]
}

-- | Creates a table.
--
-- Asserts that the number of entries in headings is equal to the number
-- of entries in each row.
unsafeTable :: [String] -> [[String]] -> Table
unsafeTable heading rows = U.assertP satisfiesInvariants $ Table heading rows

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

--------------------------------------------------------------------------------
-- Invariants
--------------------------------------------------------------------------------
satisfiesInvariants :: Table -> Bool
satisfiesInvariants t
    | length t.rows /= 0 = all (\row -> length row == length t.headings) t.rows
    | otherwise = True

