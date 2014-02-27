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
    event $ do
        reg <- newE r fr
        regI <- regE i
        genE . emit $ I_fwriter1_make reg fn regI
    return r

fwriter1Close :: Ref s FW1 -> Score s ()
fwriter1Close r = event $ do
    reg <- regE r
    genE . emit $ I_fwriter1_close reg

fwriter2Make :: FilePath -> Ref s P -> Ref s P -> Score s (Ref s FW2)
fwriter2Make fn il ir = do
    r <- freshRef TagFW2
    fr <- here
    event $ do
        reg <- newE r fr
        regL <- regE il
        regR <- regE ir
        genE . emit $ I_fwriter2_make reg fn regL regR
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

envLin :: Ref s Env -> Double -> Double -> Score s ()
envLin r x1 t = event $ do
    reg <- regE r
    genE . emit $ I_env_lin reg x1 t

envXdec :: Ref s Env -> Double -> Double -> Score s ()
envXdec r xinf tau = event $ do
    reg <- regE r
    genE . emit $ I_env_xdec reg xinf tau

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

-- vim:fdm=marker:
