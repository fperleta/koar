-- extensions {{{
-- {-# LANGUAGE #-}
-- }}}

-- exports {{{
module Koar.Pitch
    ( Pitch
    , pitchRatio
    , pitchInv
    , Transposable(..)
    , ji, ed, edo, edt, edf
    ) where
-- }}}

-- imports {{{
import           Data.Function (fix)
import           Data.List (genericLength, insert, intercalate)
import           Data.Monoid
import           Data.Ratio

import           Koar.Common
-- }}}

-- prime numbers & factorization {{{

primes :: [Integer]
primes = 2 : 3 : 5 : 7 : rest
  where
    rest = filter isPrime . dropWhile (<= 7) $ wheel [2, 3, 5, 7]

isPrime :: Integer -> Bool
isPrime n
    | n < 2 = False
    | otherwise = all (\p -> n `mod` p /= 0) . takeWhile (\p -> p^2 <= n) $ primes

wheel :: [Integer] -> [Integer]
wheel ps = tail . scanl (+) 0 $ skip : wheel
  where
    len = product ps
    bits = map (\n -> all (\p -> n `mod` p /= 0) ps) [0 .. len - 1]
    (junk, first) = break id bits
    skip = genericLength junk
    wheel = go first
    go (_:xs) = case break id xs of
        (stuff, []) -> 1 + genericLength stuff + skip : wheel
        (stuff, rs) -> 1 + genericLength stuff : go rs

trialDiv :: Integer -> [Integer]
trialDiv = go primes
  where
    go :: [Integer] -> Integer -> [Integer]
    go _ 1 = []
    go pz@(p:ps) n
        | p^2 > n = [n]
        | mod n p == 0 = p : go pz (div n p)
        | otherwise = go ps n

powers :: [Integer] -> [(Integer, Integer)]
powers [] = []
powers (x:xs) = go x 1 xs
  where
    go p k [] = (p, k) : []
    go p k (x:xs)
        | p == x     =  go p (k + 1) xs
        | otherwise  =  (p, k) : go x 1 xs

pvector :: [(Integer, Integer)] -> [Integer]
pvector = go primes
  where
    go _ [] = []
    go (p:ps) xs@((p', k):xs')
        | p == p' = k : go ps xs'
        | otherwise = 0 : go ps xs

-- }}}

-- (relative) pitch {{{

newtype Pitch = Pitch { unPitch :: [Rational] }
  deriving (Eq)

pitchRatio :: Pitch -> Double
pitchRatio (Pitch ks) = product $ zipWith f primes ks
  where
    f p k = fromIntegral p ** fromRational k



instance Show Pitch where
    show (Pitch ks) = '|' : intercalate " " (map go ks) ++ ">"
      where
        go k = showsPrec 0 (numerator k) $ if denominator k == 1
            then []
            else '/' : show (denominator k)

instance Monoid Pitch where
    mempty = Pitch []
    mappend (Pitch a) (Pitch b) = Pitch $ go a b
      where
        go [] bs = bs
        go as [] = as
        go (a:as) (b:bs) = case (a + b, go as bs) of
            (0, []) -> []
            (c, cs) -> c : cs

pitchInv :: Pitch -> Pitch
pitchInv (Pitch ks) = Pitch $ map negate ks

-- }}}

-- transposable {{{

class Transposable a where
    transpose :: Pitch -> a -> a

instance Transposable Pitch where
    transpose = mappend

-- }}}

-- just intonation {{{

ji :: Rational -> Pitch
ji q
    | q >= 0 = (Pitch . go $ numerator q)
            <> (Pitch . map negate . go $ denominator q)
    | otherwise = error "ji: negative rational!"
  where
    go = map fromIntegral . pvector . powers . trialDiv

-- }}}

-- equal temperament {{{

ed :: Rational -> Rational -> Pitch
ed period = f
  where
    Pitch ks = ji period
    f q = Pitch $ map (* q) ks

edo, edt, edf :: Rational -> Pitch
edo = ed 2
edt = ed 3
edf = ed (3/2)

-- }}}

-- vim:fdm=marker:
