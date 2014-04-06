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

    -- client
    , runInstrs

    -- slave
    , runSlave

    ) where
-- }}}

-- imports {{{
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Bits
import           Data.ByteString.Lazy.Builder
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LB
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import           Data.Monoid
import           Data.Word
import           Network.Socket (HostName, PortNumber)
import           System.Exit
import           System.IO
import           System.Process

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

    -- arrays:
    | I_array_make Reg Nat Double
    | I_array_const Reg Double
    | I_array_normalize Reg Double
    | I_array_dc Reg Double
    | I_array_partial Reg Double Nat Double
    | I_array_ghw Reg Double Double
    | I_array_bw Reg Double Double Double
    | I_array_pcw Reg Double
    | I_array_load1 String Reg
    | I_array_load2 String Reg Reg

    -- passive nodes:
    | I_sum Reg
    | I_prod Reg

    -- touches:
    | I_touch Reg Reg

    -- wires:
    | I_wire_make Reg Reg Reg Double
    | I_wire_scale Reg Double

    -- file writers:
    | I_fwriter1_make Reg String Nat Reg
    | I_fwriter1_close Reg
    | I_fwriter2_make Reg String Nat Reg Reg
    | I_fwriter2_close Reg

    -- envelope generators:
    | I_env_make Reg Reg Double
    | I_env_const Reg Double
    | I_env_lin Reg Double Double
    | I_env_xdec Reg Double Double

    -- phasors:
    | I_phasor_make Reg Reg Reg
    | I_phasor_jump Reg Double

    -- cos2pis:
    | I_cos2pi_make Reg Reg Reg

    -- lookups:
    | I_lookup_make Reg Reg Reg Reg
    | I_lookup_table Reg Reg

    -- noises:
    | I_noise_make Reg Reg Nat
    | I_noise_seed Reg Nat
    | I_noise_white Reg
    | I_noise_pink Reg

    -- dwriters:
    | I_dwriter_make Reg Reg Nat

    -- dtaps:
    | I_dtap_make Reg Reg Reg Nat
    | I_dtap_adjust Reg Nat

    -- vdelays:
    | I_vdelay_make Reg Reg Reg Reg Nat
    | I_vdelay_gains Reg Double Double Double
    | I_vdelay_freqmode Reg

    -- biquads:
    | I_biquad_make Reg Reg Reg Nat
    | I_biquad_gain Reg Double
    | I_biquad_coeffs Reg Nat Double Double Double Double

    -- blits:
    | I_blit_make Reg Reg Reg
    | I_blit_gain Reg Double
    | I_blit_jump Reg Double
    | I_blit_unipolar Reg
    | I_blit_bipolar Reg

    -- resons:
    | I_reson_make Reg Reg Reg Reg Reg
    | I_reson_pure Reg Double
    | I_reson_res Reg Double
    | I_reson_peak Reg Double
    | I_reson_lowpass Reg Double

    -- tanhs:
    | I_tanh_make Reg Reg Reg
    | I_tanh_gain Reg Double
    | I_tanh_slope Reg Double

    -- moogs:
    | I_moog_make Reg Reg Reg Reg Reg
    | I_moog_params Reg Double Double Double

    -- reverbs:
    | I_reverb_make Reg Reg Reg Reg Reg Nat Nat Nat
    | I_reverb_internal Reg Nat Nat Nat Double Double
    | I_reverb_sources Reg Nat Nat Double Double Nat Double Double
    | I_reverb_sinks Reg Nat Nat Double Double Nat Double Double
    | I_reverb_diffuse Reg Nat Nat Nat Double Nat Double Nat Double Nat Double
    | I_reverb_tcfilter Reg Double

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

    I_array_make r k x0         -> bNat 16 <> bNat r <> bNat k <> bDbl x0
    I_array_const r x0          -> bNat 17 <> bNat r <> bDbl x0
    I_array_normalize r amp     -> bNat 18 <> bNat r <> bDbl amp
    I_array_dc r offset         -> bNat 19 <> bNat r <> bDbl offset
    I_array_partial r a k th    -> bNat 20 <> bNat r <> bDbl a <> bNat k <> bDbl th
    I_array_ghw r alpha beta    -> bNat 21 <> bNat r <> bDbl alpha <> bDbl beta
    I_array_bw r a0 a1 a2       -> bNat 22 <> bNat r <> bDbl a0 <> bDbl a1 <> bDbl a2
    I_array_pcw r e             -> bNat 23 <> bNat r <> bDbl e
    I_array_load1 fn o          -> bNat 24 <> bStr fn <> bNat o
    I_array_load2 fn l r        -> bNat 25 <> bStr fn <> bNat l <> bNat r

    I_sum r                     -> bNat 64 <> bNat r
    I_prod r                    -> bNat 65 <> bNat r

    I_touch r out               -> bNat 72 <> bNat r <> bNat out

    I_wire_make r i o s         -> bNat 74 <> bNat r <> bNat i <> bNat o <> bDbl s
    I_wire_scale r s            -> bNat 75 <> bNat r <> bDbl s

    I_fwriter1_make r fn sr i   -> bNat 76 <> bNat r <> bStr fn <> bNat sr <> bNat i
    I_fwriter1_close r          -> bNat 77 <> bNat r
    I_fwriter2_make r fn sr il ir
                                -> bNat 78 <> bNat r <> bStr fn <> bNat sr <> bNat il <> bNat ir
    I_fwriter2_close r          -> bNat 79 <> bNat r

    I_env_make r out x0         -> bNat 80 <> bNat r <> bNat out <> bDbl x0
    I_env_const r x0            -> bNat 81 <> bNat r <> bDbl x0
    I_env_lin r x1 t            -> bNat 82 <> bNat r <> bDbl x1 <> bDbl t
    I_env_xdec r xinf tau       -> bNat 83 <> bNat r <> bDbl xinf <> bDbl tau

    I_phasor_make r src snk     -> bNat 90 <> bNat r <> bNat src <> bNat snk
    I_phasor_jump r phase       -> bNat 91 <> bNat r <> bDbl phase

    I_cos2pi_make r src snk     -> bNat 92 <> bNat r <> bNat src <> bNat snk

    I_lookup_make r tbl src snk -> bNat 94 <> bNat r <> bNat tbl <> bNat src <> bNat snk
    I_lookup_table r tbl        -> bNat 95 <> bNat r <> bNat tbl

    I_noise_make r snk seed     -> bNat 96 <> bNat r <> bNat snk <> bNat seed
    I_noise_seed r seed         -> bNat 97 <> bNat r <> bNat seed
    I_noise_white r             -> bNat 98 <> bNat r
    I_noise_pink r              -> bNat 99 <> bNat r

    I_dwriter_make r src size   -> bNat 100 <> bNat r <> bNat src <> bNat size

    I_dtap_make r snk from offs -> bNat 102 <> bNat r <> bNat snk <> bNat from <> bNat offs
    I_dtap_adjust r offs        -> bNat 103 <> bNat r <> bNat offs

    I_vdelay_make r src dsig snk len
                                -> bNat 104 <> bNat r <> bNat src <> bNat dsig <> bNat snk <> bNat len
    I_vdelay_gains r raw del fb -> bNat 105 <> bNat r <> bDbl raw <> bDbl del <> bDbl fb
    I_vdelay_freqmode r         -> bNat 106 <> bNat r

    I_biquad_make r src snk n   -> bNat 108 <> bNat r <> bNat src <> bNat snk <> bNat n
    I_biquad_gain r gain        -> bNat 109 <> bNat r <> bDbl gain
    I_biquad_coeffs r i b1 b2 a1 a2
                                -> bNat 110 <> bNat r <> bNat i <> bDbl b1 <> bDbl b2 <> bDbl a1 <> bDbl a2

    I_blit_make r freq snk      -> bNat 112 <> bNat r <> bNat freq <> bNat snk
    I_blit_gain r gain          -> bNat 113 <> bNat r <> bDbl gain
    I_blit_jump r phase         -> bNat 114 <> bNat r <> bDbl phase
    I_blit_unipolar r           -> bNat 115 <> bNat r
    I_blit_bipolar r            -> bNat 116 <> bNat r

    I_reson_make r src fsig qsig snk
                                -> bNat 118 <> bNat r <> bNat src <> bNat fsig <> bNat qsig <> bNat snk
    I_reson_pure r gain         -> bNat 119 <> bNat r <> bDbl gain
    I_reson_res r gain          -> bNat 120 <> bNat r <> bDbl gain
    I_reson_peak r gain         -> bNat 121 <> bNat r <> bDbl gain
    I_reson_lowpass r gain      -> bNat 122 <> bNat r <> bDbl gain

    I_tanh_make r src snk       -> bNat 124 <> bNat r <> bNat src <> bNat snk
    I_tanh_gain r gain          -> bNat 125 <> bNat r <> bDbl gain
    I_tanh_slope r slope        -> bNat 126 <> bNat r <> bDbl slope

    I_moog_make r src fsig ksig snk
                                -> bNat 128 <> bNat r <> bNat src <> bNat fsig <> bNat ksig <> bNat snk
    I_moog_params r gain drive thermal
                                -> bNat 129 <> bNat r <> bDbl gain <> bDbl drive <> bDbl thermal

    I_reverb_make r i1 i2 o1 o2 elen ecount nbr
                                -> bNat 130 <> bNat r <> bNat i1 <> bNat i2 <> bNat o1 <> bNat o2 <> bNat elen <> bNat ecount <> bNat nbr
    I_reverb_internal r w1 w2 offs g p
                                -> bNat 131 <> bNat r <> bNat w1 <> bNat w2 <> bNat offs <> bDbl g <> bDbl p
    I_reverb_sources r w loffs lg lp roffs rg rp
                                -> bNat 132 <> bNat r <> bNat w <> bNat loffs <> bDbl lg <> bDbl lp <> bNat roffs <> bDbl rg <> bDbl rp
    I_reverb_sinks r w loffs lg lp roffs rg rp
                                -> bNat 133 <> bNat r <> bNat w <> bNat loffs <> bDbl lg <> bDbl lp <> bNat roffs <> bDbl rg <> bDbl rp
    I_reverb_diffuse r w n l1 g1 l2 g2 l3 g3 l4 g4
                                -> bNat 134 <> bNat r <> bNat w <> bNat n
                                    <> bNat l1 <> bDbl g1 <> bNat l2 <> bDbl g2 <> bNat l3 <> bDbl g3 <> bNat l4 <> bDbl g4
    I_reverb_tcfilter r beta    -> bNat 135 <> bNat r <> bDbl beta

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
    regsChunk = 64

freeReg :: Reg -> Gen ()
freeReg r = Gen $ \fr is -> ((), putFReg r fr, is)

-- }}}

-- chunking {{{

chunkInstrs :: Nat -> [Instr] -> [[Instr]]
chunkInstrs smallest = go 0 []
  where
    go :: Nat -> [Instr] -> [Instr] -> [[Instr]]
    go t buf is = case is of
        [] -> [reverse buf]
        i@(I_advance dt) : is'
            | t + dt >= smallest -> reverse (i : buf) : go 0 [] is'
            | otherwise -> go (t + dt) (i : buf) is'
        i : is' -> go t (i : buf) is'

-- }}}

-- client {{{

runInstrs :: String -> [Instr] -> IO ()
runInstrs endpoint is = runDirectT endpoint $ do
    goFree
    msg <- pullMsg
    liftIO . print $ msgBody msg
    return ()
  where
    goFree = withReply "patch" $ \reply ->
        if reply == "okay"
        then goBound
        else goFree

    goBound = do
        liftIO $ putStrLn "sending the stream..."
        let cs = chunkInstrs 48000 is
        goChunks cs

    goChunks [] = do
        liftIO $ putStrLn "done."
    goChunks (c:cs) = do
        let msg = toLazyByteString . mconcat $ map bInstr c
        withReply msg $ \reply -> do
            liftIO $ print reply
            case cs of
                []  -> return ()
                _   | msg == "okay" -> goChunks cs
                    | otherwise -> goChunks cs

-- }}}

-- slave {{{

putInstrs :: Handle -> [Instr] -> IO ()
putInstrs h is = forM_ (chunkInstrs 48000 is) $ \cs -> do
    LB.hPut h . toLazyByteString . mconcat $ map bInstr cs
    hFlush h

runSlave :: [Instr] -> IO ()
runSlave is = do
    let cp = (proc "koar" ["--slave", "-v"]) { std_in = CreatePipe }
    (Just h, _, _, ph) <- createProcess cp
    hSetBuffering h NoBuffering
    hSetBinaryMode h True
    putInstrs h is
    hFlush h
    status <- waitForProcess ph
    case status of
        ExitSuccess -> return ()
        _ -> exitFailure

-- }}}

-- vim:fdm=marker:
