-- extensions {{{
-- {-# LANGUAGE #-}
-- }}}

-- exports {{{
module Koar.LTI
    ( module Data.Monoid

    , AF()
    , afGain
    , afButterLP
    , afLowShelf
    , afHighShelf
    , afPeaking

    , DF ()
    , dfGain
    , dfBlockDC
    , dfButterLP
    , dfLowShelf
    , dfHighShelf
    , dfPeaking

    , dfToSOS

    ) where
-- }}}

-- imports {{{
import           Data.Complex
import           Data.Monoid

import           Koar.Common
-- }}}

-- polynomials with real coefficients {{{

-- the leading coefficent is 1.
data RealPoly = RP
    { rpReals :: [R]
    , rpConjs :: [C]
    }
  deriving (Show)

instance Monoid RealPoly where
    mempty = RP [] []
    mappend (RP rs cs) (RP rs' cs') = RP (rs ++ rs') (cs ++ cs')
    mconcat ps = RP (concat rss) (concat css)
      where
        (rss, css) = unzip $ map (\(RP rs cs) -> (rs, cs)) ps

    {-# INLINE mempty #-}
    {-# INLINE mappend #-}
    {-# INLINE mconcat #-}



rpLinear :: R -> RealPoly
rpLinear b = RP [-b] []

rpQuadratic :: R -> R -> RealPoly
rpQuadratic b c
    | d >= 0 = RP [(-b + rd)/2, (-b - rd)/2] []
    | otherwise = RP [] [(-b / 2) :+ (sqrt (-d) / 2)]
  where
    d = b^2 - 4*c
    rd = sqrt d



rpEval :: RealPoly -> C -> C
rpEval (RP rs cs) x
    = product [ x - (r :+ 0) | r <- rs ]
    * product [ x^2 - (2 * realPart c :+ 0) * x
                    + ((magnitude c)^2 :+ 0) | c <- cs ]

rpLogMag :: RealPoly -> C -> R
rpLogMag (RP rs cs) x
    = sum [ log . magnitude $ x - (r :+ 0) | r <- rs ]
    + sum [ log . magnitude $ x^2 - (2 * realPart c :+ 0)
                                  + ((magnitude c)^2 :+ 0) | c <- cs ]

toInvCoeffs :: RealPoly -> [R]
toInvCoeffs (RP rs cs) = foldr polyProd [1]
    $ [ [1, -r] | r <- rs ]
    ++ [ [1, -2 * realPart c, (magnitude c)^2] | c <- cs ]

-- polynomial operations {{{

polySum :: (Num a) => [a] -> [a] -> [a]
polySum = go
  where
    go [] ys = ys
    go xs [] = xs
    go (x:xs) (y:ys) = x + y : go xs ys

polyProd :: (Num a) => [a] -> [a] -> [a]
polyProd = go
  where
    go [] _ = []
    go (x:xs) ys = map (x *) ys `polySum` (0 : polyProd xs ys)

-- }}}

-- }}}

-- rational functions with real coefficients {{{

data RealRat = RR
    { rrScale :: {-# UNPACK #-} !R
    , rrNumer :: {-# UNPACK #-} !RealPoly
    , rrDenom :: {-# UNPACK #-} !RealPoly
    }
  deriving (Show)

instance Monoid RealRat where
    mempty = RR 1 mempty mempty
    mappend (RR s b a) (RR s' b' a') = RR (s * s') (b <> b') (a <> a')
    mconcat rs = RR (product ss) (mconcat bs) (mconcat as)
      where
        (ss, bs, as) = unzip3 $ map (\(RR s b a) -> (s, b, a)) rs

    {-# INLINE mempty #-}
    {-# INLINE mappend #-}
    {-# INLINE mconcat #-}

rrConst :: R -> RealRat
rrConst x = RR x mempty mempty
{-# INLINE rrConst #-}



rrEval :: RealRat -> C -> C
rrEval (RR s b a) x = (s :+ 0) * rpEval b x / rpEval a x

rrLogMag :: RealRat -> C -> R
rrLogMag (RR s b a) x = log s + rpLogMag b x - rpLogMag a x

-- }}}

-- analog filters {{{

newtype AF = AF { unAF :: RealRat }

instance Monoid AF where
    mempty = AF mempty
    mappend (AF a) (AF b) = AF $ a <> b
    mconcat = AF . mconcat . map unAF

    {-# INLINE mempty #-}
    {-# INLINE mappend #-}
    {-# INLINE mconcat #-}

magAF :: AF -> R -> R
magAF (AF tf) w = magnitude . rrEval tf $ 0 :+ w

-- pure gain {{{

afGain :: R -> AF
afGain g = AF $ RR g mempty mempty

-- }}}

-- butterworth {{{

afButterLP :: R -> R -> N -> AF
afButterLP wc g n
    = AF . RR g0 mempty
    $ if even n
    then RP [] [mcis wc $ (k - 0.5) * d | k <- ks]
    else RP [-wc] [mcis wc $ k * d | k <- ks]
  where
    g0 = g * (wc ^ n)
    mcis r th = (-r * cos th) :+ (r * sin th)
    d = pi / fromIntegral n
    ks = take (fromIntegral n `div` 2) [1, 2 ..]

-- }}}

-- shelves {{{

afLowShelf :: R -> R -> AF
afLowShelf w g = AF $ RR 1 (RP [-w * g] []) (RP [-w] [])

afHighShelf :: R -> R -> AF
afHighShelf w g = AF $ RR g (RP [-w/g] []) (RP [-w] [])

-- }}}

-- peaking {{{

afPeaking :: R -> R -> AF
afPeaking g q = AF $ RR 1 (rpQuadratic (g/q) 1) (rpQuadratic (1/q) 1)

-- }}}

-- }}}

-- digital filters {{{

newtype DF = DF { unDF :: RealRat }

instance Monoid DF where
    mempty = DF mempty
    mappend (DF a) (DF b) = DF $ a <> b
    mconcat = DF . mconcat . map unDF

    {-# INLINE mempty #-}
    {-# INLINE mappend #-}
    {-# INLINE mconcat #-}

magDF :: DF -> R -> R
magDF (DF tf) f = magnitude . rrEval tf . mkPolar 1 $ 2 * pi * f

-- pure gain {{{

dfGain :: R -> DF
dfGain g = DF $ RR g mempty mempty

-- }}}

-- DC blocker {{{

dfBlockDC :: R -> DF
dfBlockDC r = DF $ RR 1 (RP [1] []) (RP [r] [])

-- }}}

-- butterworth {{{

dfButterLP :: R -> R -> N -> DF
dfButterLP fc g n = dfGain (g/dc) <> df
  where
    df = bilinear c $ afButterLP (2*pifc) 1 n
    pifc = pi * fc
    c = 2 * pifc / tan pifc
    dc = magnitude $ rrEval (unDF df) 1

testButterLP :: R -> N -> IO ()
testButterLP fc n = mapM_ line $
    [ ("DC gain", tmag 0, rmag 0)
    , ("cutoff gain", tmag fc, rmag fc)
    ] ++
    [ (show n ++ "*fc gain", tmag (n*fc), rmag (n*fc))
    | n <- takeWhile (< 0.5 * recip fc) [2 .. 20]
    ]
  where
    tf = unDF $ dfButterLP fc 1 n
    tmag f = recip . sqrt $ 1 + (f/fc)^(2*n)
    rmag f = magnitude . rrEval tf . mkPolar 1 $ 2 * pi * f
    line (s, t, r) = putStrLn $ concat
        [ s, ": theoretical = ", dBshow t
        , "dB; real = ", dBshow r
        ]

-- }}}

-- shelves {{{

dfLowShelf :: R -> R -> DF
dfLowShelf f g = dfGain (g/dc) <> df
  where
    df = bilinear c $ afLowShelf (2*pif) g
    pif = pi * f
    c = 2 * pif / tan pif
    dc = magnitude $ rrEval (unDF df) 1

dfHighShelf :: R -> R -> DF
dfHighShelf f g = dfGain (g/g0) <> df
  where
    df = bilinear c $ afHighShelf (2*pif) g
    pif = pi * f
    c = 2 * pif / tan pif
    g0 = magnitude $ rrEval (unDF df) (-1)

-- }}}

-- peaking {{{

dfPeaking :: R -> R -> R -> DF
dfPeaking f g q = dfGain (1/g0) <> df
  where
    df = bilinear c $ afPeaking g q
    pif = pi * f
    c = recip $ tan pif
    mag = magnitude . rrEval (unDF df)
    g0 = min (mag 1) (mag (-1))

-- }}}

-- }}}

-- bilinear transform {{{

bilinear :: R -> AF -> DF
bilinear k (AF (RR s b a)) = DF $ RR (s * gb * ga) (b' <> cb) (a' <> ca)
  where
    (gb, pb, b') = bilinPoly k b
    (ga, pa, a') = bilinPoly k a

    cb = mconcat $ replicate (max 0 $ pa - pb) (RP [-1] [])
    ca = mconcat $ replicate (max 0 $ pb - pa) (RP [-1] [])

bilinReal :: R -> R -> (R, R)
bilinReal k r = (k - r, (k + r) / (k - r))

bilinConj :: R -> C -> (R, C)
bilinConj k (a :+ b) =
    ( k2 - 2*k*a + a2 + b2
    , ((k2-a2-b2) / d) :+ (2*k*b / d)
    )
  where
    k2 = k^2; a2 = a^2; b2 = b^2
    d = k2 - 2*k*a + a2 + b2

bilinPoly :: R -> RealPoly -> (R, Int, RealPoly)
bilinPoly k (RP rs cs) =
    ( product gs * product gs'
    , length rs + 2 * length cs
    , RP rs' cs'
    )
  where
    (gs, rs') = unzip $ map (bilinReal k) rs
    (gs', cs') = unzip $ map (bilinConj k) cs

-- }}}

-- second order sections {{{

type TwoCoeffs = (R, R) -- b1, b2
type SOS = (R, R, R, R) -- b1, b2, a1, a2

coeffsConj :: C -> TwoCoeffs
coeffsConj c = (-2 * realPart c, (magnitude c)^2)

coeffsRealPair :: R -> R -> TwoCoeffs
coeffsRealPair r1 r2 = (negate $ r1 + r2, r1 * r2)

coeffsReal :: R -> TwoCoeffs
coeffsReal r = (-r, 0)

coeffsPoly :: RealPoly -> [TwoCoeffs]
coeffsPoly (RP rs cs) = go rs
  where
    go [] = map coeffsConj cs
    go [r] = coeffsReal r : map coeffsConj cs
    go (r1:r2:rs') = coeffsRealPair r1 r2 : go rs'

dfToSOS :: DF -> (R, [SOS])
dfToSOS (DF (RR s b a)) = (s, go (coeffsPoly b) (coeffsPoly a))
  where
    go [] as = map (\(a1, a2) -> (0, 0, a1, a2)) as
    go bs [] = map (\(b1, b2) -> (b1, b2, 0, 0)) bs
    go ((b1, b2):bs) ((a1, a2):as) = (b1, b2, a1, a2) : go bs as

-- }}}

-- plotting {{{

logScale :: Int -> Int -> R -> R -> R
logScale 0 _ _ _ = 0
logScale i n c s = c * 2 ** (log s * (2 * k - 1) / log 2)
  where
    k = fromIntegral i / fromIntegral n

plotMags :: Int -> [R] -> IO ()
plotMags cols logmags = do
    putStrLn $ "bottom: " ++ show bottom ++ "dB; top: " ++ show top ++ "dB"
    mapM_ (putStrLn . row) dbs
  where
    dbs = map ((*) $ 20 / log 10) logmags
    top = maximum dbs
    bottom = max (-90) . minimum $ filter (not . isInfinite) dbs
    range = max 1 $ top - bottom
    step = range / fromIntegral cols
    row m
        | m - bottom < step/2 = ""
        | m - bottom < step = ">"
        | otherwise = '#' : row (m - step)

plotAF :: Int -> Int -> R -> R -> AF -> IO ()
plotAF rows cols wc s (AF tf) = plotMags cols
    [ rrLogMag tf $ 0 :+ w
    | i <- [0 .. rows - 1]
    , let w = logScale i (rows - 1) wc s
    ]

plotDF :: Int -> Int -> R -> DF -> IO ()
plotDF rows cols fc (DF tf) = plotMags cols
        [ rrLogMag tf $ cos th :+ sin th
        | i <- [0 .. rows - 1]
        , let th = logScale i (rows - 1) fc (1/fc)
        ]

-- }}}

-- vim:fdm=marker:
