-- extensions {{{
{-# LANGUAGE DataKinds, GADTs #-}
-- }}}

-- exports {{{
module Koar.PadSynth
    ( PadPartial(..)
    , PadSound()
    , padSound
    , padOscil
    , padOscil2
    , padPlot
    ) where
-- }}}

-- imports {{{
import           Control.Monad
import           Graphics.EasyPlot

import           Koar.Common
import           Koar.Flow
import           Koar.Patch
import           Koar.Score
-- }}}

-- types {{{

data PadPartial = PadPartial
    { padGain :: R
    , padRatio :: R
    , padSpread :: R
    }

data PadSound s = PadSound
    { padArray :: Ref s Array
    , padSize :: Nat
    , padCycles :: Nat
    }

-- }}}

-- rendering {{{

bands :: Nat -> [PadPartial] -> [R]
bands cycles = go . map pbands
  where
    f0 = fromIntegral cycles
    diff xs = zipWith subtract xs (tail xs)
    go xss = case unzip [(x, xs) | x:xs <- xss] of
        (xs, xss') -> sum xs : go xss'

    pbands (PadPartial g r s) =
        [ (g / (s' * sqrt (2 * pi))) * exp (negate . (^2) $ (x-r') / (2*s'))
        | x <- map fromIntegral $ enumFrom 1
        , let r' = r * f0
        , let s' = s * f0
        ]

padSound :: Nat -> Nat -> Nat -> [PadPartial] -> Score s (PadSound s)
padSound size cycles seed ps = do
    arr <- arrayMake size 0

    let items = zip3
            [1 .. size `div` 2 - 1]
            (bands cycles ps)
            phases

    forM_ items $ \(k, a, ph) -> do
        when (a > 1e-9) $ arrayPartial arr a k ph

    arrayDC arr 0
    arrayNormalize arr 1

    return PadSound
        { padArray = arr
        , padSize = size
        , padCycles = cycles
        }

  where
    f0 = fromIntegral cycles

    lcg k = (1103515245 * k + 12345) `mod` (2^31)
    phase k = let x = pi * fromIntegral k / 100000000
              in x - fromIntegral (floor x)
    phases = map phase $ iterate lcg seed

-- }}}

-- oscillators {{{

padOscil :: PadSound s -> R -> Pipe s Mono Mono
padOscil pad ph0 = Pipe $ \(Sink1 out) -> do
    phase <- makeSum
    lookupMake (padArray pad) phase out
    freq <- makeSum
    phasor <- phasorMake freq phase
    phasorJump phasor ph0
    inp <- makeSum
    wireMake inp freq . recip . fromIntegral $ padCycles pad
    return $ Sink1 inp

padOscil2 :: PadSound s -> R -> Pipe s Mono Stereo
padOscil2 pad ph0 = Pipe $ \(Sink2 outL outR) -> do
    phaseL <- makeSum
    phaseR <- makeSum
    lookupMake (padArray pad) phaseL outL
    lookupMake (padArray pad) phaseR outR
    freq <- makeSum
    phasorMake freq phaseL >>= flip phasorJump ph0
    wireMake phaseL phaseR 1
    envMake phaseR $ 1 / sqrt 3
    inp <- makeSum
    wireMake inp freq . recip . fromIntegral $ padCycles pad
    return $ Sink1 inp

-- }}}

-- plotting {{{

padPlot :: Nat -> Nat -> [PadPartial] -> IO Bool
padPlot cycles width ps = plot' [Interactive] X11
    [ Data2D [Style Lines, Color White] [] vs ]
  where
    vs = zip [1..fromIntegral $ width*cycles]
        . map (max (-120) . (*10) . logBase 10)
        $ bands cycles ps

-- }}}

-- vim:fdm=marker:

