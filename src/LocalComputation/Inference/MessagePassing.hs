-- TODO: !!!!! PUT THE PROBLEM OF REDUNDANT COMBINATIONS IN FUTURE WORK AS A POSSIBLE SPEED IMPROVEMENT !!!!!

module LocalComputation.Inference.MessagePassing (
    Mode (..)
) where


data Mode = Threads | Distributed deriving (Eq, Show)
