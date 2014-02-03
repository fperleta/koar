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

-- }}}

-- vim:fdm=marker:
