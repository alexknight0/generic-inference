
module ValuationAlgebra.QuasiRegular
    (
    )
where

-- import           Data.Matrix as M
import qualified Data.Map as M



--data QuasiRegularValuation a b = QuasiRegularValuation (M.Matrix b)

-- | A Quasi-regular semiring is also known as a 'closed' semiring
class QuasiRegularSemiringValue a where
    add :: a -> a -> a
    multiply :: a -> a -> a
    inverse :: a -> a

    -- Also has to satisfy some special laws with
    -- respect to idempotency and monotonicity to
    -- work for path problems? pdf page 257 gen. inf.
    --additiveIdentity :: a
    --multiplicativeIdentity :: a



