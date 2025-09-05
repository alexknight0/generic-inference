module LocalComputation.Pretty (
    showTable
) where

import qualified LocalComputation.Utils as U
import           Text.PrettyPrint.Boxes as B

-- TODO: cant we put U.table here?

table :: U.Table -> Box
table (U.Table [] _) = nullBox
table t              = hsep 2 left [column, table rest]
    where
        column = vsep 1 left [text (head t.heading), vcat left (map (text . head) t.rows)]
        rest = t { U.heading = tail t.heading
                 , U.rows    = map tail t.rows
                }

showTable :: U.Table -> String
showTable = render . table


foobar :: ()
foobar = undefined
{-
>>> render $ (//) (text "foo") (text "foobar")
"foo   \nfoobar\n"

>>> render $ vcat left $ map text ["foo", "foooest", "f", "fass"]
"foo    \nfoooest\nf      \nfass   \n"

-}
