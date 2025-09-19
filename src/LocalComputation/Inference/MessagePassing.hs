-- TODO: !!!!! PUT THE PROBLEM OF REDUNDANT COMBINATIONS IN FUTURE WORK AS A POSSIBLE SPEED IMPROVEMENT !!!!!
-- TODO: !!!!! INVESTIGATE THE FACT THAT A LOT OF EXPENSIVE PROJECTIONS ARE MADE DURING DISTRIBUTE !!!!!

module LocalComputation.Inference.MessagePassing (
    Mode (..)
) where


data Mode = Threads | Distributed deriving (Eq, Show)
