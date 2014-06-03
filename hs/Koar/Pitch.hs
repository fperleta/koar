-- extensions {{{
-- {-# LANGUAGE #-}
-- }}}

-- exports {{{
module Koar.Pitch
    where
-- }}}

-- imports {{{
import           Data.Function (fix)

import           Koar.Common
import           Koar.Score
-- }}}

-- pitch spaces {{{

data Pitch = Pitch
    { pitchHertz :: {-# UNPACK #-} !Double
    , pitchUp :: Pitch
    , pitchDown :: Pitch
    }

pitchFreq :: Pitch -> Freq
pitchFreq = hz . pitchHertz

pitchSpace :: (Int -> Double) -> Pitch
pitchSpace f = p0
  where
    p0 = Pitch (f 0) (up 1 p0) (down (-1) p0)
    up k p' = fix $ \x -> Pitch (f k) (up (succ k) x) p'
    down k p' = fix $ \x -> Pitch (f k) p' (down (pred k) x)

pitchUpward :: Pitch -> [Freq]
pitchUpward = go
  where
    go x = pitchFreq x : go (pitchUp x)

-- }}}

-- basic operations {{{

transpose :: Int -> Pitch -> Pitch
transpose k p = case compare k 0 of
    EQ -> p
    LT -> transpose (succ k) $ pitchDown p
    GT -> transpose (pred k) $ pitchUp p

relativeTo :: Pitch -> [Int] -> [Freq]
relativeTo p ks = map (pitchFreq . flip transpose p) ks

-- }}}

-- equal temperament {{{

equalTemperament :: Rational -> Nat -> Pitch -> Pitch
equalTemperament b t (Pitch f0 _ _) = pitchSpace $ \k ->
    f0 * (fromRational b ** (fromIntegral k / fromIntegral t))

tet12, tet19 :: Pitch -> Pitch
tet12 = equalTemperament 2 12
tet19 = equalTemperament 2 19

midiTuning :: Pitch
midiTuning = transpose (-69) . tet12 . pitchSpace $ const 440

-- }}}

-- just intervals {{{

circleOfFifths :: Pitch -> Pitch
circleOfFifths (Pitch f0 _ _) = pitchSpace $ \k ->
    f0 * (fromIntegral (2 ^ min 0 k * 3 ^ max 0 k)
        / fromIntegral (2 ^ max 0 k * 3 ^ min 0 k))

-- }}}

-- vim:fdm=marker:
