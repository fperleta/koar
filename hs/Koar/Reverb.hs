-- extensions {{{
{-# LANGUAGE DataKinds, GADTs #-}
-- }}}

-- exports {{{
module Koar.Reverb
    where
-- }}}

-- imports {{{
import           Data.Complex
import           Data.List
import           Graphics.EasyPlot

import           Koar.Common
import           Koar.Flow
import           Koar.Patch
import           Koar.Score
-- }}}

-- types {{{

data ReverbSpec = ReverbSpec
    { rsNumWalls :: N
    , rsWallDists :: [[R]]
    , rsWallGains :: [[R]]
    , rsWallWidths :: [R]
    , rsSrcDists :: ([R], [R])
    , rsSrcGains :: ([R], [R])
    , rsSnkDists :: ([R], [R])
    , rsTimeDC :: Time
    , rsTimeNyq :: Time
    }

-- }}}

-- instantiation {{{

reverb :: ReverbSpec -> Pipe s Stereo Stereo
reverb rs = Pipe $ \(Sink2 o1 o2) -> do
    i1 <- makeSum; i2 <- makeSum

    let nwalls = rsNumWalls rs
    spm <- toPeriods' . sec $ recip speedOfSound -- samples per meter

    let wds = map (map (* spm)) $ rsWallDists rs
    let wps = zipWith zip wds $ rsWallGains rs
    let wts = map (* spm) $ rsWallWidths rs
    let lids = map (* spm) . fst $ rsSrcDists rs
    let rids = map (* spm) . snd $ rsSrcDists rs
    let lps = zip lids . fst $ rsSrcGains rs
    let rps = zip rids . snd $ rsSrcGains rs
    let lods = map (* spm) . fst $ rsSnkDists rs
    let rods = map (* spm) . snd $ rsSnkDists rs

    nDC <- toPeriods' $ rsTimeDC rs
    nNyq <- toPeriods' $ rsTimeNyq rs

    let damping g0 m = let
            { r0 = exp $ -6.91 * m / nDC
            ; rP = exp $ -6.91 * m / nNyq
            ; g = g0 * 2 * r0 * rP / (r0 + rP)
            ; p = (r0 - rP) / (r0 + rP)
            } in (g, p)
    let diffusion l t = (exp $ -6.91 * fromIntegral l / t) / sqrt 2

    let maxWd = maximum (map maximum wds) `max` maximum lids `max` maximum rids
    let maxSd = maximum lods `max` maximum rods

    rev <- reverbMake i1 i2 o1 o2 nwalls (ceiling maxWd + 10) (ceiling maxSd + 10)

    -- internal reflections:
    forM_ (zip [0..] wps) $ \(w1, ps) -> do
        forM_ (zip [0..] ps) $ \(w2, (m, g0)) -> when (w1 /= w2) $ do
            let (g, p) = damping g0 m
            reverbInternal rev w1 w2 (round m) g p

    -- sources:
    forM_ (zip3 [0..] lps rps) $ \(w, (lm, lg0), (rm, rg0)) -> do
        let (lg, lp) = damping lg0 lm
        let (rg, rp) = damping rg0 rm
        reverbSources rev w (round lm) lg lp (round rm) rg rp

    -- sinks:
    forM_ (zip3 [0..] lods rods) $ \(w, lm, rm) -> do
        let (lg, lp) = damping 1 lm
        let (rg, rp) = damping 1 rm
        reverbSinks rev w (round lm) lg lp (round rm) rg rp

    -- diffusers:
    forM_ (zip [0..] wts) $ \(w, t) -> do
        let ls@[l1, l2, l3, l4] = [13, 17, 19, 23]
        let [g1, g2, g3, g4] = map (flip diffusion t) ls
        reverbDiffuse rev w 4 l1 g1 l2 g2 l3 g3 l4 g4

    -- tonal correction:
    let alpha = nNyq / nDC
    let beta = (1 - alpha) / (1 + alpha)
    reverbTCFilter rev beta

    return $ Sink2 i1 i2

-- }}}

-- construction {{{

speedOfSound :: Rational
speedOfSound = 343 -- in m/s

type Point = C
type Wall = (C, C)

roundRoom :: N -> R -> [Wall]
roundRoom nwalls diameter = take n $ zip ps' (tail ps')
  where
    n = fromIntegral nwalls
    ps = [mkPolar (diameter/2) (2 * pi * i / fromIntegral n) + mkPolar 0.1 (i^2 / fromIntegral n) | i <- [0..]]
    ps' = cycle $ take n ps

seenFrom :: Wall -> Point -> R -- gives an angle in radians
seenFrom (w1, w2) p = abs . acos $ (a^2 + b^2 - c^2) / (2 * a * b)
  where
    a = magnitude $ w1 - p
    b = magnitude $ w2 - p
    c = magnitude $ w1 - w2

buildReverb :: Time -> Time -> R -> R -> R -> [Wall] -> ReverbSpec
buildReverb tDC tNyq isep osep front ws = ReverbSpec
    { rsNumWalls = fromIntegral $ length ws
    , rsWallDists = [[dist c1 c2 | c2 <- cs] | c1 <- cs]
    , rsWallGains = [[wallGain w c | w <- ws] | c <- cs]
    , rsWallWidths = [dist p1 p2 | (p1, p2) <- ws]
    , rsSrcDists = unzip [(dist il c, dist ir c) | c <- cs]
    , rsSrcGains = unzip [(srcGain w il, srcGain w ir) | w <- ws]
    , rsSnkDists = unzip [(dist ol c, dist or c) | c <- cs]
    , rsTimeDC = tDC
    , rsTimeNyq = tNyq
    }
  where
    ix = front / 3; ox = -2 * front / 3
    il = ix :+ (-isep/2); ir = ix :+ (isep/2)
    ol = ox :+ (-osep/2); or = ox :+ (osep/2)
    cs = map ((/ 2) . uncurry (+)) ws
    dist a b = magnitude $ a - b
    wallGain w c = (w `seenFrom` c) / pi
    srcGain w p = (w `seenFrom` p) / (2 * pi)

-- }}}

-- model {{{

plotSig :: Int -> [R] -> IO Bool
plotSig n xs
    = plot X11 . Data2D [Style Impulses] []
    . zip [0..] $ take n xs

plotIR :: Int -> ([R] -> [R]) -> IO Bool
plotIR n h = plotSig n $ h impulse

plotRR :: Int -> Int -> ([R] -> [R]) -> IO Bool
plotRR n k h = plotSig n . h $ replicate k 1 ++ repeat 0

plotED :: Int -> ([R] -> [R]) -> IO Bool
plotED n h = plotSig n . map (^2) $ h impulse

impulse :: [R]
impulse = 1 : repeat 0

delay :: N -> [R] -> [R]
delay 0 = id
delay n = (0 :) . delay (n - 1)

diffuse :: N -> R -> [R] -> [R]
diffuse n g xs = zipWith (+) old (map (* negate g) new)
  where
    old = delay n new
    new = zipWith (+) xs (map (* g) old)

damp :: R -> R -> [R] -> [R]
damp g p xs = ys
  where
    ys = zipWith (+) (map (* g) xs) $ map (* p) (0 : ys)

dampDelay :: R -> R -> N -> [R] -> [R]
dampDelay n60dc n60nyq n = damp g p . delay n
  where
    dc = exp $ -6.91 * fromIntegral n / n60dc
    nyq = exp $ -6.91 * fromIntegral n / n60nyq
    g = 2 * dc * nyq / (dc + nyq)
    p = (dc - nyq) / (dc + nyq)

tcFilter :: R -> R -> [R] -> [R]
tcFilter n60dc n60nyq xs = zipWith f xs (0 : xs)
  where
    f x0 x1 = (x0 - b * x1) / (1 - b)
    b = (1 - a) / (1 + a)
    a = n60nyq / n60dc

allpass :: [R] -> [R] -> [R]
allpass [] xs = xs
allpass (k:ks) xs = allpass ks $ zipWith (+) old (map (* negate k) new)
  where
    old = 0 : new
    new = zipWith (+) xs (map (* k) old)

-- }}}

-- vim:fdm=marker:
