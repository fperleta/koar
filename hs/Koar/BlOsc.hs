-- extensions {{{
{-# LANGUAGE DataKinds, GADTs #-}
-- }}}

-- exports {{{
module Koar.BlOsc
    where
-- }}}

-- imports {{{
import           Control.Monad

import           Koar.Common
import           Koar.Flow
import           Koar.Patch
import           Koar.Score
-- }}}

-- square waves {{{

sqOsc :: Time -> Pipe s Mono Mono
sqOsc tau = leaky +: osc
  where
    osc = blit BipolarBlit
    leaky = pipe1 $ \out -> do
        inp <- makeSum
        bq <- biquadMake inp out 1
        biquadGain bq 2
        tau' <- toPeriods' tau
        let alpha = exp . negate $ recip tau'
        biquadCoeffs bq 0 0 0 (negate alpha) 0
        return inp

-- }}}

-- triangle waves {{{

triOsc :: Time -> Pipe s Mono Mono
triOsc tau = Pipe $ \(Sink1 out) -> do
    tmp@(Sink1 m) <- monoProd
    inp <- graft tmp $ sqOsc tau
    tmp <: (1 `from` inp)
    bq <- biquadMake m out 1
    biquadGain bq 1
    tau' <- toPeriods' tau
    let alpha = exp . negate $ recip tau'
    biquadCoeffs bq 0 0 0 (negate $ 2 * alpha) (alpha^2)
    return inp

-- }}}

-- vim:fdm=marker:
