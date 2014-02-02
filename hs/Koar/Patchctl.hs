-- extensions {{{
-- {-# LANGUAGE #-}
-- }}}

-- exports {{{
module Koar.Patchctl
    where
-- }}}

-- imports {{{
import           Control.Monad
import           Data.Bits
import           Data.ByteString.Builder
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LB
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import           Data.Monoid
import           Data.Word
-- }}}

-- basic types {{{

type Nat = Word
type Reg = Word

data Instr
    = I_nop
    | I_leave
    | I_resize Nat
    | I_blank Reg
    | I_move Reg Reg
    | I_dup Reg Reg
    | I_advance Nat
    | I_sum Reg
    | I_prod Reg
  deriving (Eq, Show)

-- }}}

-- serialization {{{

bNat :: Nat -> Builder
bNat 0 = word8 0
bNat n = mconcat . map (word8 . toEnum . fst) . reverse $ takeWhile ((/= 0).snd)
    [ (fromEnum $ ((n `shiftR` k) .&. 0x7F) .|. f, n `shiftR` k)
    | (k, f) <- zip [0, 7 ..] $ 0 : repeat 0x80 ]

bInt :: Int -> Builder
bInt z = bNat . toEnum $ if z >= 0
    then 2 * z
    else -2 * z - 1;

bDbl :: Double -> Builder
bDbl = doubleBE

bInstr :: Instr -> Builder
bInstr x = case x of
    I_nop               -> bNat 0
    I_leave             -> bNat 1
    I_resize k          -> bNat 2 <> bNat k
    I_blank r           -> bNat 3 <> bNat r
    I_move s d          -> bNat 4 <> bNat s <> bNat d
    I_dup s d           -> bNat 5 <> bNat s <> bNat d
    I_advance t         -> bNat 6 <> bNat t
    I_sum r             -> bNat 7 <> bNat r
    I_prod r            -> bNat 8 <> bNat r

-- }}}

-- free registers {{{

data FRegs = FRegs
    { numRegs :: {-# UNPACK #-} !Int
    , freeRegs :: IntSet
    }

initFRegs :: FRegs
initFRegs = FRegs 0 IS.empty

growFRegs :: Int -> FRegs -> FRegs
growFRegs k (FRegs n fs) = FRegs n' $ IS.union fs fs'
  where
    n' = n + k
    fs' = IS.fromAscList [n..n'-1]

shrinkFRegs :: Int -> FRegs -> FRegs
shrinkFRegs k (FRegs n fs) = FRegs n' $ IS.difference fs fs'
  where
    n' = n - k
    fs' = IS.fromAscList [n'..n-1]

getFReg :: FRegs -> Maybe (Reg, FRegs)
getFReg (FRegs n fs) = fmap f $ IS.minView fs
  where
    f (r, fs') = (toEnum r, FRegs n fs')

putFReg :: Reg -> FRegs -> FRegs
putFReg r (FRegs n fs) = FRegs n $ IS.insert (fromEnum r) fs

-- }}}

-- generators {{{

newtype Gen a = Gen { unGen :: FRegs -> [Instr] -> (a, FRegs, [Instr]) }

instance Functor Gen where
    fmap f gen = Gen $ \fr is -> case unGen gen fr is of
        (x, fr', is') -> (f x, fr', is')
    {-# INLINE fmap #-}

instance Monad Gen where
    return x = Gen $ \fr is -> (x, fr, is)
    {-# INLINE return #-}

    gen >>= f = Gen $ \fr is -> let
        { (x, fr', is'') = unGen gen fr is'
        ; (fx, fr'', is') = unGen (f x) fr' is
        } in (fx, fr'', is'')
    {-# INLINE (>>=) #-}

    gen >> gen' = Gen $ \fr is -> let
        { (_, fr', is'') = unGen gen fr is'
        ; (x, fr'', is') = unGen gen' fr' is
        } in (x, fr'', is'')
    {-# INLINE (>>) #-}

runGen :: Gen () -> [Instr]
runGen gen = case unGen gen initFRegs [I_resize 0, I_leave] of
    (_, fr, is) -> is

emit :: Instr -> Gen ()
emit x = Gen $ \fr is -> ((), fr, x:is)

allocReg :: Gen Reg
allocReg = Gen $ \fr is -> case getFReg fr of
    Just (r, fr') -> (r, fr', is)
    Nothing -> let
        { size = numRegs fr + regsChunk
        ; gen = emit (I_resize $ toEnum size) >> allocReg
        } in unGen gen (growFRegs regsChunk fr) is
  where
    regsChunk = 4

freeReg :: Reg -> Gen ()
freeReg r = Gen $ \fr is -> ((), putFReg r fr, is)

-- }}}

-- vim:fdm=marker:
