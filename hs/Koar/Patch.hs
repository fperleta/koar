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

-- vim:fdm=marker:
