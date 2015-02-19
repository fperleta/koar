-- extensions {{{
{-# LANGUAGE DataKinds #-}
-- }}}

-- exports {{{
module Koar.Patch
    where
-- }}}

-- imports {{{
import           Koar.Patchctl
import           Koar.Score
-- }}}

-- arrays {{{

arrayMake :: Nat -> Double -> Score s (Ref s Array)
arrayMake size x0 = do
    r <- freshRef TagArray
    fr <- here
    event $ do
        reg <- newE r fr
        genE . emit $ I_array_make reg size x0
    return r

arrayConst :: Ref s Array -> Double -> Score s ()
arrayConst r x0 = event $ do
    reg <- regE r
    genE . emit $ I_array_const reg x0

arrayNormalize :: Ref s Array -> Double -> Score s ()
arrayNormalize r amp = event $ do
    reg <- regE r
    genE . emit $ I_array_normalize reg amp

arrayDC :: Ref s Array -> Double -> Score s ()
arrayDC r offset = event $ do
    reg <- regE r
    genE . emit $ I_array_dc reg offset

arrayPartial :: Ref s Array -> Double -> Nat -> Double -> Score s ()
arrayPartial r amp index phase = event $ do
    reg <- regE r
    genE . emit $ I_array_partial reg amp index phase

arrayGHW :: Ref s Array -> Double -> Double -> Score s ()
arrayGHW r alpha beta = event $ do
    reg <- regE r
    genE . emit $ I_array_ghw reg alpha beta

arrayBW :: Ref s Array -> Double -> Double -> Double -> Score s ()
arrayBW r a0 a1 a2 = event $ do
    reg <- regE r
    genE . emit $ I_array_bw reg a0 a1 a2

arrayPCW :: Ref s Array -> Double -> Score s ()
arrayPCW r e = event $ do
    reg <- regE r
    genE . emit $ I_array_pcw reg e

arrayLoad1 :: String -> Score s (Ref s Array)
arrayLoad1 fn = do
    r <- freshRef TagArray
    fr <- here
    event $ do
        reg <- newE r fr
        genE . emit $ I_array_load1 fn reg
    return r

arrayLoad2 :: String -> Score s (Ref s Array, Ref s Array)
arrayLoad2 fn = do
    rL <- freshRef TagArray
    rR <- freshRef TagArray
    fr <- here
    event $ do
        regL <- newE rL fr
        regR <- newE rR fr
        genE . emit $ I_array_load2 fn regL regR
    return (rL, rR)

-- }}}

-- passive nodes {{{

makeSum :: Score s (Ref s P)
makeSum = do
    r <- freshRef TagP
    fr <- here
    event $ do
        reg <- newE r fr
        genE . emit $ I_sum reg
    return r

makeProd :: Score s (Ref s P)
makeProd = do
    r <- freshRef TagP
    fr <- here
    event $ do
        reg <- newE r fr
        genE . emit $ I_prod reg
    return r

-- }}}

-- touch {{{

touch :: Ref s P -> Score s ()
touch out = do
    r <- freshRef TagEnv
    fr <- here
    event $ do
        reg <- newE r fr
        regOut <- regE out
        genE . emit $ I_touch reg regOut
    return ()

-- }}}

-- wire {{{

wireMake :: Ref s P -> Ref s P -> Double -> Score s (Ref s Wire)
wireMake i o s = do
    r <- freshRef TagWire
    fr <- here
    event $ do
        reg <- newE r fr
        regI <- regE i
        regO <- regE o
        genE . emit $ I_wire_make reg regI regO s
    return r

wireScale :: Ref s Wire -> Double -> Score s ()
wireScale r s = event $ do
    reg <- regE r
    genE . emit $ I_wire_scale reg s

-- }}}

-- fwriter {{{

fwriter1Make :: FilePath -> Ref s P -> Score s (Ref s FW1)
fwriter1Make fn i = do
    r <- freshRef TagFW1
    fr <- here
    sr <- fromIntegral <$> sampleRate
    event $ do
        reg <- newE r fr
        regI <- regE i
        genE . emit $ I_fwriter1_make reg fn sr regI
    return r

fwriter1Close :: Ref s FW1 -> Score s ()
fwriter1Close r = event $ do
    reg <- regE r
    genE . emit $ I_fwriter1_close reg

fwriter2Make :: FilePath -> Ref s P -> Ref s P -> Score s (Ref s FW2)
fwriter2Make fn il ir = do
    r <- freshRef TagFW2
    fr <- here
    sr <- fromIntegral <$> sampleRate
    event $ do
        reg <- newE r fr
        regL <- regE il
        regR <- regE ir
        genE . emit $ I_fwriter2_make reg fn sr regL regR
    return r

fwriter2Close :: Ref s FW2 -> Score s ()
fwriter2Close r = event $ do
    reg <- regE r
    genE . emit $ I_fwriter2_close reg

-- }}}

-- env {{{

envMake :: Ref s P -> Double -> Score s (Ref s Env)
envMake out x0 = do
    r <- freshRef TagEnv
    fr <- here
    event $ do
        reg <- newE r fr
        regOut <- regE out
        genE . emit $ I_env_make reg regOut x0
    return r

envConst :: Ref s Env -> Double -> Score s ()
envConst r x0 = event $ do
    reg <- regE r
    genE . emit $ I_env_const reg x0

envLin :: Ref s Env -> Double -> Time -> Score s ()
envLin r x1 t = do
    p <- toPeriods' t
    event $ do
        reg <- regE r
        genE . emit $ I_env_lin reg x1 p

envXdec :: Ref s Env -> Double -> Time -> Score s ()
envXdec r xinf tau = do
    p <- toPeriods' tau
    event $ do
        reg <- regE r
        genE . emit $ I_env_xdec reg xinf p

envCos :: Ref s Env -> Double -> Time -> Score s ()
envCos r x1 t = do
    p <- toPeriods' t
    event $ do
        reg <- regE r
        genE . emit $ I_env_cos reg x1 p

-- }}}

-- phasor {{{

phasorMake :: Ref s P -> Ref s P -> Score s (Ref s Phasor)
phasorMake src snk = do
    r <- freshRef TagPhasor
    fr <- here
    event $ do
        reg <- newE r fr
        regSrc <- regE src
        regSnk <- regE snk
        genE . emit $ I_phasor_make reg regSrc regSnk
    return r

phasorJump :: Ref s Phasor -> Double -> Score s ()
phasorJump self phase = event $ do
    reg <- regE self
    genE . emit $ I_phasor_jump reg phase

-- }}}

-- cos2pi {{{

cos2piMake :: Ref s P -> Ref s P -> Score s (Ref s Cos2pi)
cos2piMake src snk = do
    r <- freshRef TagCos2pi
    fr <- here
    event $ do
        reg <- newE r fr
        regSrc <- regE src
        regSnk <- regE snk
        genE . emit $ I_cos2pi_make reg regSrc regSnk
    return r

-- }}}

-- lookup {{{

lookupMake :: Ref s Array -> Ref s P -> Ref s P -> Score s (Ref s Lookup)
lookupMake tbl src snk = do
    r <- freshRef TagLookup
    fr <- here
    event $ do
        reg <- newE r fr
        regTbl <- regE tbl
        regSrc <- regE src
        regSnk <- regE snk
        genE . emit $ I_lookup_make reg regTbl regSrc regSnk
    return r

lookupTable :: Ref s Lookup -> Ref s Array -> Score s ()
lookupTable r tbl = event $ do
    reg <- regE r
    regTbl <- regE tbl
    genE . emit $ I_lookup_table reg regTbl

-- }}}

-- noise {{{

noiseMake :: Ref s P -> Nat -> Score s (Ref s Noise)
noiseMake snk seed = do
    r <- freshRef TagNoise
    fr <- here
    event $ do
        reg <- newE r fr
        regSnk <- regE snk
        genE . emit $ I_noise_make reg regSnk seed
    return r

noiseSeed :: Ref s Noise -> Nat -> Score s ()
noiseSeed r seed = event $ do
    reg <- regE r
    genE . emit $ I_noise_seed reg seed

noiseWhite :: Ref s Noise -> Score s ()
noiseWhite r = event $ do
    reg <- regE r
    genE . emit $ I_noise_white reg

noisePink :: Ref s Noise -> Score s ()
noisePink r = event $ do
    reg <- regE r
    genE . emit $ I_noise_pink reg

-- }}}

-- dwriter {{{

dwriterMake :: Ref s P -> Time -> Score s (Ref s DW)
dwriterMake src t = do
    periods <- floor <$> toPeriods' t
    r <- freshRef TagDW
    fr <- here
    event $ do
        reg <- newE r fr
        regSrc <- regE src
        genE . emit $ I_dwriter_make reg regSrc periods
    return r

-- }}}

-- dtap {{{

dtapMake :: Ref s P -> Ref s DW -> Time -> Score s (Ref s DTap)
dtapMake snk from t = do
    offs <- floor <$> toPeriods' t
    r <- freshRef TagDTap
    fr <- here
    event $ do
        reg <- newE r fr
        regSnk <- regE snk
        regFrom <- regE from
        genE . emit $ I_dtap_make reg regSnk regFrom offs
    return r

dtapAdjust :: Ref s DTap -> Time -> Score s ()
dtapAdjust tap t = do
    offs <- floor <$> toPeriods' t
    event $ do
        reg <- regE tap
        genE . emit $ I_dtap_adjust reg offs

-- }}}

-- vdelay {{{

vdelayMake :: Ref s P -> Ref s P -> Ref s P -> Time -> Score s (Ref s VDelay)
vdelayMake src dsig snk len = do
    periods <- floor <$> toPeriods' len
    r <- freshRef TagVDelay
    fr <- here
    event $ do
        reg <- newE r fr
        regSrc <- regE src
        regDsig <- regE dsig
        regSnk <- regE snk
        genE . emit $ I_vdelay_make reg regSrc regDsig regSnk periods
    return r

vdelayGains :: Ref s VDelay -> Double -> Double -> Double -> Score s ()
vdelayGains vd raw del fb = event $ do
    reg <- regE vd
    genE . emit $ I_vdelay_gains reg raw del fb

vdelayFreqMode :: Ref s VDelay -> Score s ()
vdelayFreqMode vd = event $ do
    reg <- regE vd
    genE . emit $ I_vdelay_freqmode reg

-- }}}

-- biquad {{{

biquadMake :: Ref s P -> Ref s P -> Nat -> Score s (Ref s Biquad)
biquadMake src snk stages = do
    r <- freshRef TagBiquad
    fr <- here
    event $ do
        reg <- newE r fr
        regSrc <- regE src
        regSnk <- regE snk
        genE . emit $ I_biquad_make reg regSrc regSnk stages
    return r

biquadGain :: Ref s Biquad -> Double -> Score s ()
biquadGain bq gain = event $ do
    reg <- regE bq
    genE . emit $ I_biquad_gain reg gain

biquadCoeffs :: Ref s Biquad -> Nat -> Double -> Double -> Double -> Double -> Score s ()
biquadCoeffs bq stage b1 b2 a1 a2 = event $ do
    reg <- regE bq
    genE . emit $ I_biquad_coeffs reg stage b1 b2 a1 a2

-- }}}

-- blit {{{

blitMake :: Ref s P -> Ref s P -> Score s (Ref s Blit)
blitMake freq snk = do
    r <- freshRef TagBlit
    fr <- here
    event $ do
        reg <- newE r fr
        regFreq <- regE freq
        regSnk <- regE snk
        genE . emit $ I_blit_make reg regFreq regSnk
    return r

blitGain :: Ref s Blit -> Double -> Score s ()
blitGain blit g = event $ do
    reg <- regE blit
    genE . emit $ I_blit_gain reg g

blitJump :: Ref s Blit -> Double -> Score s ()
blitJump blit phase = event $ do
    reg <- regE blit
    genE . emit $ I_blit_jump reg phase

blitUnipolar :: Ref s Blit -> Score s ()
blitUnipolar blit = event $ genE . emit . I_blit_unipolar =<< regE blit

blitBipolar :: Ref s Blit -> Score s ()
blitBipolar blit = event $ genE . emit . I_blit_bipolar =<< regE blit

-- }}}

-- reson {{{

resonMake :: Ref s P -> Ref s P -> Ref s P -> Ref s P -> Score s (Ref s Reson)
resonMake src fsig qsig snk = do
    r <- freshRef TagReson
    fr <- here
    event $ do
        reg <- newE r fr
        regSrc <- regE src
        regFsig <- regE fsig
        regQsig <- regE qsig
        regSnk <- regE snk
        genE . emit $ I_reson_make reg regSrc regFsig regQsig regSnk
    return r

data ResonMode
    = Pure2Pole
    | ConstResGain
    | ConstPeakGain
    | LowPass2Pole
    | HighPass2Pole

resonMode :: Ref s Reson -> ResonMode -> Double -> Score s ()
resonMode rs mode gain = event $ do
    let opc = case mode of
            Pure2Pole -> I_reson_pure
            ConstResGain -> I_reson_res
            ConstPeakGain -> I_reson_peak
            LowPass2Pole -> I_reson_lowpass
            HighPass2Pole -> I_reson_highpass
    reg <- regE rs
    genE . emit $ opc reg gain

-- }}}

-- tanh {{{

tanhMake :: Ref s P -> Ref s P -> Score s (Ref s Tanh)
tanhMake src snk = do
    r <- freshRef TagTanh
    fr <- here
    event $ do
        reg <- newE r fr
        regSrc <- regE src
        regSnk <- regE snk
        genE . emit $ I_tanh_make reg regSrc regSnk
    return r

tanhGain :: Ref s Tanh -> Double -> Score s ()
tanhGain th gain = event $ do
    reg <- regE th
    genE . emit $ I_tanh_gain reg gain

tanhSlope :: Ref s Tanh -> Double -> Score s ()
tanhSlope th slope = event $ do
    reg <- regE th
    genE . emit $ I_tanh_slope reg slope

-- }}}

-- moog {{{

moogMake :: Ref s P -> Ref s P -> Ref s P -> Ref s P -> Score s (Ref s Moog)
moogMake src fsig ksig snk = do
    r <- freshRef TagMoog
    fr <- here
    event $ do
        reg <- newE r fr
        regSrc <- regE src
        regFsig <- regE fsig
        regKsig <- regE ksig
        regSnk <- regE snk
        genE . emit $ I_moog_make reg regSrc regFsig regKsig regSnk
    return r

moogParams :: Ref s Moog -> Double -> Double -> Double -> Score s ()
moogParams moog gain drive thermal = event $ do
    reg <- regE moog
    genE . emit $ I_moog_params reg gain drive thermal

-- }}}

-- reverb {{{

reverbMake :: Ref s P -> Ref s P -> Ref s P -> Ref s P -> Nat -> Nat -> Nat -> Score s (Ref s Reverb)
reverbMake i1 i2 o1 o2 nwalls wlen slen = do
    r <- freshRef TagReverb
    fr <- here
    event $ do
        reg <- newE r fr
        regI1 <- regE i1
        regI2 <- regE i2
        regO1 <- regE o1
        regO2 <- regE o2
        genE . emit $ I_reverb_make reg regI1 regI2 regO1 regO2 nwalls wlen slen
    return r

reverbInternal :: Ref s Reverb -> Nat -> Nat -> Nat -> Double -> Double -> Score s ()
reverbInternal rev w1 w2 offs g p = event $ do
    reg <- regE rev
    genE . emit $ I_reverb_internal reg w1 w2 offs g p

reverbSources :: Ref s Reverb -> Nat -> Nat -> Double -> Double -> Nat -> Double -> Double -> Score s ()
reverbSources rev w loffs lg lp roffs rg rp = event $ do
    reg <- regE rev
    genE . emit $ I_reverb_sources reg w loffs lg lp roffs rg rp

reverbSinks :: Ref s Reverb -> Nat -> Nat -> Double -> Double -> Nat -> Double -> Double -> Score s ()
reverbSinks rev w loffs lg lp roffs rg rp = event $ do
    reg <- regE rev
    genE . emit $ I_reverb_sinks reg w loffs lg lp roffs rg rp

reverbDiffuse :: Ref s Reverb -> Nat -> Nat -> Nat -> Double -> Nat -> Double  -> Nat -> Double -> Nat -> Double -> Score s ()
reverbDiffuse rev w nstages l1 g1 l2 g2 l3 g3 l4 g4 = event $ do
    reg <- regE rev
    genE . emit $ I_reverb_diffuse reg w nstages l1 g1 l2 g2 l3 g3 l4 g4

reverbTCFilter :: Ref s Reverb -> Double -> Score s ()
reverbTCFilter rev beta = event $ do
    reg <- regE rev
    genE . emit $ I_reverb_tcfilter reg beta

-- }}}

-- vim:fdm=marker:
