{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module LocalComputation.Utils.Composition where

--------------------------------------------------------------------------------
-- Multiple function composition
--------------------------------------------------------------------------------
-- Count the dots after the first dot to figure out how many times we compose.
-- Inspired by https://hackage.haskell.org/package/composition-1.0.2.1/docs/Data-Composition.html
{-# INLINE (.:) #-}
(.:) :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
a .: b = (a .) . b
infixr 8 .:

{-# INLINE (.:.) #-}
(.:.) :: (b -> c) -> (a1 -> a2 -> a3 -> b) -> a1 -> a2 -> a3 -> c
a .:. b = ((a .) .) . b
infixr 8 .:.

{-# INLINE (.::) #-}
(.::) :: (b -> c) -> (a1 -> a2 -> a3 -> a4 -> b) -> a1 -> a2 -> a3 -> a4 -> c
a .:: b = (((a .) .) .) . b
infixr 8 .::

{-# INLINE (.::.) #-}
(.::.) :: (b -> c) -> (a1 -> a2 -> a3 -> a4 -> a5 -> b) -> a1 -> a2 -> a3 -> a4 -> a5 -> c
a .::. b = ((((a .) .) .) .) . b
infixr 8 .::.

{-# INLINE (.:::) #-}
(.:::) :: (b -> c) -> (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> b) -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> c
a .::: b = (((((a .) .) .) .) .) . b
infixr 8 .:::

{-# INLINE (.:::.) #-}
(.:::.) :: (b -> c) -> (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> b) -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> c
a .:::. b = ((((((a .) .) .) .) .) .) . b
infixr 8 .:::.

{-# INLINE (.::::) #-}
(.::::) :: (b -> c) -> (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> b) -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> c
a .:::: b = (((((((a .) .) .) .) .) .) .) . b
infixr 8 .::::


