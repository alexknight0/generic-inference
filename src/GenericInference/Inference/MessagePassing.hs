module GenericInference.Inference.MessagePassing (
    Mode (..)
) where


data Mode = Threads | Distributed deriving (Eq, Show)
