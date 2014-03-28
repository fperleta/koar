-- extensions {{{
{-# LANGUAGE DataKinds, GADTs #-}
-- }}}

-- exports {{{
module Koar.PdOsc
    ( pdOsc
    ) where
-- }}}

-- imports {{{
import           Control.Monad

import           Koar.Common
import           Koar.Flow
import           Koar.Patch
import           Koar.Score
-- }}}

-- distortion units {{{

type DistUnit s = Ref s P -> Ref s P -> Score s ()
-- cos2piMake :: DistUnit

pdLayer :: N -> Ref s P -> Ref s P -> DistUnit s -> DistUnit s
pdLayer k dsig osig inner src snk = do

    -- input to the distortion unit
    phi1 <- makeSum
    wireMake src phi1 $ fromIntegral k
    wireMake osig phi1 1

    -- output from the distortion unit
    dphi <- makeProd
    inner phi1 dphi
    wireMake dsig dphi . recip $ 2 * pi * fromIntegral k

    -- distorted phase
    phi <- makeSum
    wireMake src phi 1
    wireMake dphi phi 1

    -- cosine oscillator
    cos2piMake phi snk

    return ()

-- }}}

-- oscillators {{{

pdOsc :: [(N, Sink s Mono, Sink s Mono)] -> Pipe s Mono Mono
pdOsc layers = Pipe $ \(Sink1 out) -> do
    inp <- makeSum
    ph0 <- makeSum
    phasorMake inp ph0
    go layers ph0 out
    return $ Sink1 inp
  where
    go [] = \src snk -> void $ cos2piMake src snk
    go ((k, Sink1 dsig, Sink1 osig):ls) = pdLayer k dsig osig (go ls)

-- }}}

-- vim:fdm=marker:
