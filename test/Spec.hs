import qualified BayesianTests as B
import qualified FourierTests  as F
import qualified UtilsTests    as U



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
hedgehog output to a file. This can be done with `stack test > foo.txt`.
-}

main :: IO ()
main = B.tests >> F.tests >> U.tests >> pure ()


