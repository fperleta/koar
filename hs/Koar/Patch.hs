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

-- vim:fdm=marker:
