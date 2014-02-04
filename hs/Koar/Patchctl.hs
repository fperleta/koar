-- extensions {{{
{-# LANGUAGE OverloadedStrings #-}
-- }}}

-- exports {{{
module Koar.Patchctl

    -- imports
    ( module Control.Monad

    -- types
    , Nat
    , Reg
    , Instr(..)

    -- generators
    , Gen()
    , runGen
    , emit
    , allocReg
    , freeReg

    ) where
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
import           Network.Socket (HostName, PortNumber)

import           Koar.Proto
-- }}}

-- basic types {{{

type Nat = Word
type Reg = Word

data Instr
    -- builtins:
    = I_nop
    | I_leave
    | I_resize Nat
    | I_blank Reg
    | I_move Reg Reg
    | I_dup Reg Reg
    | I_advance Nat

    -- passive nodes:
    | I_sum Reg
    | I_prod Reg

    -- wires:
    | I_wire_make Reg Reg Reg Double
    | I_wire_scale Reg Double

    -- file writers:
    | I_fwriter1_make Reg String Reg
    | I_fwriter1_close Reg
    | I_fwriter2_make Reg String Reg Reg
    | I_fwriter2_close Reg

    -- envelope generators:
    | I_env_make Reg Reg Double
    | I_env_const Reg Double
    | I_env_lin Reg Double Double
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

bStr :: String -> Builder
bStr s = bNat (fromIntegral $ LB.length bs) <> lazyByteString bs
  where
    bs = toLazyByteString $ stringUtf8 s

bInstr :: Instr -> Builder
bInstr x = case x of
    I_nop                       -> bNat 0
    I_leave                     -> bNat 1
    I_resize k                  -> bNat 2 <> bNat k
    I_blank r                   -> bNat 3 <> bNat r
    I_move s d                  -> bNat 4 <> bNat s <> bNat d
    I_dup s d                   -> bNat 5 <> bNat s <> bNat d
    I_advance t                 -> bNat 6 <> bNat t

    I_sum r                     -> bNat 8 <> bNat r
    I_prod r                    -> bNat 9 <> bNat r

    I_wire_make r i o s         -> bNat 16 <> bNat r <> bNat i <> bNat o <> bDbl s
    I_wire_scale r s            -> bNat 17 <> bNat r <> bDbl s

    I_fwriter1_make r fn i      -> bNat 18 <> bNat r <> bStr fn <> bNat i
    I_fwriter1_close r          -> bNat 19 <> bNat r
    I_fwriter2_make r fn il ir  -> bNat 20 <> bNat r <> bStr fn <> bNat il <> bNat ir
    I_fwriter2_close r          -> bNat 21 <> bNat r

    I_env_make r out x0         -> bNat 22 <> bNat r <> bNat out <> bDbl x0
    I_env_const r x0            -> bNat 23 <> bNat r <> bDbl x0
    I_env_lin r x1 t            -> bNat 24 <> bNat r <> bDbl x1 <> bDbl t

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

-- client {{{

numberOfRetries :: Int
numberOfRetries = 1

runInstrs :: HostName -> PortNumber -> [Instr] -> IO ()
runInstrs host port is = runDirectT host port goFree
  where
    goFree = withReply "patch" $ \reply ->
        if reply == "okay"
        then goBound
        else goFree

    goBound = forM_ is $ noReply . toLazyByteString . bInstr

-- }}}

-- vim:fdm=marker:
