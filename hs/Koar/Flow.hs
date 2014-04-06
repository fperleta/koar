-- extensions {{{
{-# LANGUAGE DataKinds, GADTs, KindSignatures, ScopedTypeVariables #-}
-- }}}

-- exports {{{
module Koar.Flow
    where
-- }}}

-- imports {{{
import           Control.Monad
import           Data.Monoid

import           Koar.Common
import           Koar.Equalizer
import           Koar.Patch
import           Koar.Score
-- }}}

-- types {{{

data Chans
    = Mono
    | Stereo
  deriving (Show, Eq, Ord, Enum, Bounded)

data Sink :: * -> Chans -> * where
    Sink1 :: Ref s P -> Sink s Mono
    Sink2 :: Ref s P -> Ref s P -> Sink s Stereo

monoRefP :: Sink s Mono -> Ref s P
monoRefP (Sink1 m) = m

leftRefP, rightRefP :: Sink s Stereo -> Ref s P
leftRefP (Sink2 l _) = l
rightRefP (Sink2 _ r) = r

data Src :: * -> Chans -> * where
    Src :: (Sink s ch -> Score s ()) -> Src s ch

data Pipe :: * -> Chans -> Chans -> * where
    Pipe :: (Sink s ch' -> Score s (Sink s ch)) -> Pipe s ch ch'

-- }}}

-- shorthand constructors {{{

src1 :: (Ref s P -> Score s ()) -> Src s Mono
src1 f = Src $ \(Sink1 m) -> f m

src2 :: (Ref s P -> Ref s P -> Score s ()) -> Src s Stereo
src2 f = Src $ \(Sink2 l r) -> f l r

pipe1 :: (Ref s P -> Score s (Ref s P)) -> Pipe s Mono Mono
pipe1 f = Pipe $ \(Sink1 m) -> Sink1 <$> f m

pipeN :: (Ref s P -> Score s (Ref s P)) -> Pipe s ch ch
pipeN f = Pipe $ \snk -> case snk of
    Sink1 m -> Sink1 <$> f m
    Sink2 l r -> Sink2 <$> f l <*> f r

-- }}}

-- glue {{{

monoSum, monoProd :: Score s (Sink s Mono)
monoSum = Sink1 <$> makeSum
monoProd = Sink1 <$> makeProd

stereoSum, stereoProd :: Score s (Sink s Stereo)
stereoSum = Sink2 <$> makeSum <*> makeSum
stereoProd = Sink2 <$> makeProd <*> makeProd

(<:) :: Sink s ch -> Src s ch -> Score s ()
snk <: Src f = f snk
infix 2 <:
{-# INLINE (<:) #-}

(-:) :: Pipe s ch ch' -> Src s ch -> Src s ch'
Pipe f -: src = Src $ \snk -> do
    tmp <- f snk
    tmp <: src
infixr 3 -:
{-# INLINE (-:) #-}

graft :: Sink s ch' -> Pipe s ch ch' -> Score s (Sink s ch)
graft snk (Pipe f) = f snk
{-# INLINE graft #-}

(+:) :: Pipe s ch' ch'' -> Pipe s ch ch' -> Pipe s ch ch''
g +: f = Pipe $ \snk -> do
    snk' <- graft snk g
    graft snk' f
infixr 4 +:
{-# INLINE (+:) #-}

instance Monoid (Src s ch) where
    mempty = Src $ \snk -> case snk of
        Sink1 m -> touch m
        Sink2 l r -> touch l >> touch r
    mappend s s' = Src $ \snk -> snk <: s >> snk <: s'
    mconcat ss = Src $ \snk -> mapM_ (snk <:) ss

-- }}}

-- sinks {{{

-- dwriter {{{

data DelayLine :: * -> Chans -> * where
    DL1 :: Ref s DW -> DelayLine s Mono
    DL2 :: Ref s DW -> Ref s DW -> DelayLine s Stereo

dwriter1 :: Time -> Score s (Sink s Mono, DelayLine s Mono)
dwriter1 t = do
    m <- makeSum
    dw <- dwriterMake m t
    return (Sink1 m, DL1 dw)

dwriter2 :: Time -> Score s (Sink s Stereo, DelayLine s Stereo)
dwriter2 t = do
    l <- makeSum; r <- makeSum
    dwl <- dwriterMake l t; dwr <- dwriterMake r t
    return (Sink2 l r, DL2 dwl dwr)

-- }}}

-- fwriter {{{

fwriter1 :: FilePath -> Score s (Sink s Mono)
fwriter1 fn = do
    inp@(Sink1 m) <- monoSum
    fwriter1Make fn m
    return inp

fwriter2 :: FilePath -> Score s (Sink s Stereo)
fwriter2 fn = do
    inp@(Sink2 l r) <- stereoSum
    fwriter2Make fn l r
    return inp

-- }}}

-- }}}

-- sources {{{

-- dtap {{{

dtap :: DelayLine s ch -> Time -> Src s ch
dtap dl t = Src $ \snk -> case (snk, dl) of
    (Sink1 m, DL1 dw) -> void $ dtapMake m dw t
    (Sink2 l r, DL2 dw dw') -> void $ dtapMake l dw t >> dtapMake r dw' t

-- }}}

-- from {{{

from :: R -> Sink s ch -> Src s ch'
from gain (Sink1 m) = Src $ \snk -> case snk of
    Sink1 m' -> void $ wireMake m m' gain
    Sink2 l' r' -> void $ wireMake m l' gain >> wireMake m r' gain
from gain (Sink2 l r) = Src $ \snk -> case snk of
    Sink1 m' -> void $ wireMake l m' gain >> wireMake r m' gain
    Sink2 l' r' -> void $ wireMake l l' gain >> wireMake r r' gain

-- }}}

-- noise {{{

data NoiseColor
    = WhiteNoise
    | PinkNoise

noise :: forall s ch. NoiseColor -> N -> Src s ch
noise color seed = Src $ \snk -> case snk of
    Sink1 m -> go m seed
    Sink2 l r -> go l seed >> go r (seed + 1)
  where
    go :: Ref s P -> N -> Score s ()
    go out s = do
        n <- noiseMake out s
        case color of
            WhiteNoise -> return ()
            PinkNoise -> noisePink n

-- }}}

-- }}}

-- pipes {{{

-- blit {{{

data BlitPolarity
    = UnipolarBlit
    | BipolarBlit

blit :: BlitPolarity -> Pipe s ch ch
blit pol = blit' pol 1 0

blit' :: BlitPolarity -> R -> R -> Pipe s ch ch
blit' pol gain phi0 = pipeN $ \out -> do
    inp <- makeSum
    bl <- blitMake inp out
    blitGain bl gain
    blitJump bl phi0
    case pol of
        UnipolarBlit -> blitUnipolar bl
        BipolarBlit -> blitBipolar bl
    return inp

-- }}}

-- cos2pi {{{

cos2pi :: Pipe s ch ch
cos2pi = pipeN $ \out -> do
    inp <- makeSum
    cos2piMake inp out
    return inp

-- }}}

-- eq {{{

eq :: [EqStage] -> Pipe s ch ch
eq ss = pipeN $ \out -> do
    inp <- makeSum
    eqMake inp out ss
    return inp

-- }}}

-- lookup {{{

lookup :: Ref s Array -> Pipe s ch ch
lookup tbl = pipeN $ \out -> do
    inp <- makeSum
    lookupMake tbl inp out
    return inp

-- }}}

-- moog {{{

moog :: Sink s Mono -> Sink s Mono -> Pipe s ch ch
moog (Sink1 fsig) (Sink1 ksig) = pipeN $ \out -> do
    inp <- makeSum
    moogMake inp fsig ksig out
    return inp

-- }}}

-- phasor {{{

phasor :: R -> Pipe s ch ch
phasor phi0 = pipeN $ \out -> do
    inp <- makeSum
    ph <- phasorMake inp out
    phasorJump ph phi0
    return inp

-- }}}

-- reson {{{

reson :: ResonMode -> R -> Sink s Mono -> Sink s Mono -> Pipe s ch ch
reson mode gain (Sink1 fsig) (Sink1 qsig) = pipeN $ \out -> do
    inp <- makeSum
    rs <- resonMake inp fsig qsig out
    resonMode rs mode gain
    return inp

-- }}}

-- staticPan {{{

-- pan ∈ [-1, 1] -- left to right
-- x = sin (π pan / 2)
--
-- gains:
-- * left = sqrt ((1 - x)/2)
-- * right = sqrt (((1 + x)/2)
--
-- powers:
-- * left = (1 - x)/2
-- * right = (1 + x)/2
-- * total = (1 - x + 1 + x)/2 = 2/2 = 1 = 0dB
--
-- when pan = 0, both gains are 1/sqrt(2) = -3dB.

staticPan :: R -> R -> Pipe s Mono Stereo
staticPan gain pan = Pipe $ \(Sink2 l r) -> do
    m <- makeSum
    wireMake m l gL
    wireMake m r gR
    return $ Sink1 m
  where
    x = sin (pi * pan / 2)
    gL = gain * sqrt ((1 - x) / 2)
    gR = gain * sqrt ((1 + x) / 2)

-- }}}

-- tanhShaper {{{

tanhShaper :: R -> R -> Pipe s ch ch
tanhShaper gain slope = pipeN $ \out -> do
    inp <- makeSum
    th <- tanhMake inp out
    tanhGain th gain
    tanhSlope th slope
    return inp

-- }}}

-- vdelay {{{

vdelayTime, vdelayFreq :: forall s ch. Sink s ch -> Time -> R -> R -> R -> Pipe s ch ch

vdelayTime t len raw del fb = case t of
    Sink1 m -> Pipe $ \(Sink1 m') -> do
        inp <- makeSum
        go inp m m'
        return $ Sink1 inp
    Sink2 l r -> Pipe $ \(Sink2 l' r') -> do
        inpl <- makeSum; inpr <- makeSum
        go inpl l l'; go inpr r r'
        return $ Sink2 inpl inpr
  where
    go :: Ref s P -> Ref s P -> Ref s P -> Score s ()
    go src dsig snk = do
        vd <- vdelayMake src dsig snk len
        vdelayGains vd raw del fb

vdelayFreq f len raw del fb = case f of
    Sink1 m -> Pipe $ \(Sink1 m') -> do
        inp <- makeSum
        go inp m m'
        return $ Sink1 inp
    Sink2 l r -> Pipe $ \(Sink2 l' r') -> do
        inpl <- makeSum; inpr <- makeSum
        go inpl l l'; go inpr r r'
        return $ Sink2 inpl inpr
  where
    go :: Ref s P -> Ref s P -> Ref s P -> Score s ()
    go src fsig snk = do
        vd <- vdelayMake src fsig snk len
        vdelayGains vd raw del fb
        vdelayFreqMode vd

-- }}}

-- wire {{{

wire :: R -> Pipe s ch ch
wire gain = Pipe $ \snk -> case snk of
    Sink1 m -> do
        m' <- makeSum
        wireMake m' m gain
        return $ Sink1 m'
    Sink2 l r -> do
        l' <- makeSum; r' <- makeSum
        wireMake l' l gain
        wireMake r' r gain
        return $ Sink2 l' r'

-- }}}

-- }}}

-- envelopes {{{

data EnvStep a
    = ConstE a
    | LinE a Time
    | XdecE a Time

class EnvQuant a where
    toAmp :: EnvStep a -> Score s (EnvStep R)
    envStep :: Ref s Env -> EnvStep a -> Score s ()
    envStep r s = envStep r =<< toAmp s

instance EnvQuant Double where
    toAmp = return
    envStep r s = case s of
        ConstE x0 -> envConst r x0
        LinE x1 t -> envLin r x1 t
        XdecE xinf tau -> envXdec r xinf tau

instance EnvQuant Time where
    toAmp s = case s of
        ConstE t0 -> ConstE <$> toPeriods' t0
        LinE t' t -> do t'' <- toPeriods' t'; return $ LinE t'' t
        XdecE t tau -> do t' <- toPeriods' t; return $ XdecE t' tau

instance EnvQuant Freq where
    toAmp s = case s of
        ConstE f0 -> ConstE <$> toNormFreq' f0
        LinE f t -> do f' <- toNormFreq' f; return $ LinE f' t
        XdecE f tau -> do f' <- toNormFreq' f; return $ XdecE f' tau

-- }}}

-- vim:fdm=marker:
