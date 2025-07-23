{- | Tests for the localcomputation library.

Placed inside the library as Haskell Language Server can't dynamically reload the library when editing another component.
No functions in the library should import any functionality from 'Tests' modules.
-}
module Tests
    ( allTests
    )
where

import qualified Tests.BayesianNetwork           as B
import qualified Tests.FastFourierTransform      as F
import qualified Tests.LabelledMatrix            as M
import qualified Tests.ShortestPath.SingleTarget as S
import qualified Tests.Utils                     as U

{- | Hedgehog console output uses display regions rather than regular stdout logging, so may overwrite your stdout messages.

To ensure consistent logging while displaying stdout, follow the following template:

    import System.IO.Silently (capture)

    ...

    prop_example = ... $ do
    ...
    (msg, x) <- liftIO $ capture $ ...
    annotate msg
    ...

Source: https://github.com/hedgehogqa/haskell-hedgehog/issues/236

However, this may not work if your code spawns another process that does the logging. In this case, consider redirecting the
hedgehog output to a file, so only the logging output remains. This can be done with `stack test > /dev/null`.
-}
allTests :: IO ()
allTests = M.tests >> pure () -- B.tests >> F.tests >> M.tests >> S.tests >> U.tests >> pure ()
