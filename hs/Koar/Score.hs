-- extensions {{{
{-# LANGUAGE DataKinds, DeriveFunctor, GADTs, GeneralizedNewtypeDeriving, KindSignatures, RankNTypes #-}
-- }}}

-- exports {{{
module Koar.Score

    -- imports
    ( module Control.Applicative
    , module Control.Monad
    , module Koar.Patchctl

    -- quantities
    , Time(..), sec, msec
    , scaleTime
    , Freq(..), hz
    , scaleFreq

    -- resource kinds
    , Kind(..)
    , Tag(..)

    -- refs
    , Ref()

    -- frames
    , FRef()

    -- events
    , Event()
    , genE
    , newE
    , killE
    , regE
    , beginE
    , endE

    -- scores
    , Score()
    , runScore
    , freshRef
    , freshFRef
    , shift
    , scale
    , event
    , here
    , frame

    , sampleRate
    , toPeriods, toPeriods'
    , toNormFreq, toNormFreq'
    , timeToFreq, freqToTime

    ) where
-- }}}

-- imports {{{
import           Control.Applicative
import           Control.Monad
import           Control.Monad.State
import qualified Data.IntMap as IM
import           Data.IntMap (IntMap)
import qualified Data.IntSet as IS
import           Data.IntSet (IntSet)
import           Data.Monoid
import           Data.Ratio

import           Koar.Patchctl
-- }}}

-- internal time {{{

newtype Secs = Secs { unSecs :: Rational }
  deriving (Eq, Ord, Enum, Num, Fractional, Show)

newtype Units = Units { unUnits :: Rational }
  deriving (Eq, Ord, Enum, Num, Fractional, Show)

-- }}}

-- quantities {{{

data Time = Time
    { timeSecs   ::  {-# UNPACK #-} !Rational
    , timeCells  ::  {-# UNPACK #-} !Rational
    }
  deriving (Eq)

sec :: Rational -> Time
sec t = Time t 0
{-# INLINE sec #-}

msec :: Rational -> Time
msec t = Time (t / 1000) 0
{-# INLINE msec #-}

instance Num Time where
    Time s1 c1 + Time s2 c2 = Time (s1 + s2) (c1 + c2)
    {-# INLINE (+) #-}
    Time s1 c1 - Time s2 c2 = Time (s1 - s2) (c1 - c2)
    {-# INLINE (-) #-}
    negate (Time s c) = Time (negate s) (negate c)
    {-# INLINE negate #-}
    fromInteger = Time 0 . fromInteger
    {-# INLINE fromInteger #-}

    (*) = error "cannot multiply quantities of time"
    abs = error "abs is undefined for time"
    signum = error "signum is undefined for time"

instance Fractional Time where
    fromRational = Time 0
    {-# INLINE fromRational #-}

    (/) = error "cannot divide quantities of time"
    recip = error "recip is undefined for time"

scaleTime :: Rational -> Time -> Time
scaleTime a (Time s c) = Time (a * s) (a * c)
{-# INLINE scaleTime #-}



data Freq = Freq
    { freqHertz    ::  {-# UNPACK #-} !Double
    , freqPerCell  ::  {-# UNPACK #-} !Rational
    }
  deriving (Eq)

hz :: Double -> Freq
hz f = Freq f 0
{-# INLINE hz #-}

instance Num Freq where
    Freq h1 p1 + Freq h2 p2 = Freq (h1 + h2) (p1 + p2)
    {-# INLINE (+) #-}
    Freq h1 p1 - Freq h2 p2 = Freq (h1 - h2) (p1 - p2)
    {-# INLINE (-) #-}
    negate (Freq h p) = Freq (negate h) (negate p)
    {-# INLINE negate #-}
    fromInteger = Freq 0 . fromInteger
    {-# INLINE fromInteger #-}

    (*) = error "cannot multiply quantities of frequency"
    abs = error "abs is undefined for frequencies"
    signum = error "signum is undefined for frequencies"

instance Fractional Freq where
    fromRational = Freq 0 . fromRational
    {-# INLINE fromRational #-}

    (/) = error "cannot divide quantities of frequency"
    recip = error "recip is undefined for frequencies"

scaleFreq :: Rational -> Freq -> Freq
scaleFreq a (Freq h p) = Freq (fromRational a * h) (a * p)

-- }}}

-- resource kinds {{{

-- data Kind {{{

data Kind
    = Array
    | P -- passive nodes
    | Wire
    | FW1
    | FW2
    | Env
    | Phasor
    | Cos2pi
    | Lookup
    | Noise
    | DW
    | DTap
    | VDelay
    | Biquad
    | Blit
    | Reson
    | Tanh
    | Moog
    | Reverb

-- }}}

-- data Tag {{{

data Tag :: Kind -> * where
    TagArray :: Tag Array
    TagP :: Tag P
    TagWire :: Tag Wire
    TagFW1 :: Tag FW1
    TagFW2 :: Tag FW2
    TagEnv :: Tag Env
    TagPhasor :: Tag Phasor
    TagCos2pi :: Tag Cos2pi
    TagLookup :: Tag Lookup
    TagNoise :: Tag Noise
    TagDW :: Tag DW
    TagDTap :: Tag DTap
    TagVDelay :: Tag VDelay
    TagBiquad :: Tag Biquad
    TagBlit :: Tag Blit
    TagReson :: Tag Reson
    TagTanh :: Tag Tanh
    TagMoog :: Tag Moog
    TagReverb :: Tag Reverb

-- }}}

-- data TagProd {{{

data TagProd a = TagProd
    { tpArray :: a
    , tpP :: a
    , tpWire :: a
    , tpFW1 :: a
    , tpFW2 :: a
    , tpEnv :: a
    , tpPhasor :: a
    , tpCos2pi :: a
    , tpLookup :: a
    , tpNoise :: a
    , tpDW :: a
    , tpDTap :: a
    , tpVDelay :: a
    , tpBiquad :: a
    , tpBlit :: a
    , tpReson :: a
    , tpTanh :: a
    , tpMoog :: a
    , tpReverb :: a
    }

diagTP :: a -> TagProd a
diagTP x = TagProd x x x x x x x x x x x x x x x x x x x

mapTP :: (forall k. (IsKind k) => Tag k -> a -> b) -> TagProd a -> [b]
mapTP f tp =
    [ f TagArray $ tpArray tp
    , f TagP $ tpP tp
    , f TagWire $ tpWire tp
    , f TagFW1 $ tpFW1 tp
    , f TagFW2 $ tpFW2 tp
    , f TagEnv $ tpEnv tp
    , f TagPhasor $ tpPhasor tp
    , f TagCos2pi $ tpCos2pi tp
    , f TagLookup $ tpLookup tp
    , f TagNoise $ tpNoise tp
    , f TagDW $ tpDW tp
    , f TagDTap $ tpDTap tp
    , f TagVDelay $ tpVDelay tp
    , f TagBiquad $ tpBiquad tp
    , f TagBlit $ tpBlit tp
    , f TagReson $ tpReson tp
    , f TagTanh $ tpTanh tp
    , f TagMoog $ tpMoog tp
    , f TagReverb $ tpReverb tp
    ]

-- }}}

-- class IsKind {{{

class IsKind k where
    kindTag :: Tag k
    getTP :: Tag k -> TagProd a -> a
    setTP :: Tag k -> a -> TagProd a -> TagProd a

modifyTP :: (IsKind k) => Tag k -> (a -> a) -> TagProd a -> TagProd a
modifyTP tag f tp = setTP tag (f $ getTP tag tp) tp
{-# INLINE modifyTP #-}

-- }}}

-- instances {{{

instance IsKind Array where
    kindTag = TagArray
    getTP _ = tpArray
    setTP _ x tp = tp { tpArray = x }

instance IsKind P where
    kindTag = TagP
    getTP _ = tpP
    setTP _ x tp = tp { tpP = x }

instance IsKind Wire where
    kindTag = TagWire
    getTP _ = tpWire
    setTP _ x tp = tp { tpWire = x }

instance IsKind FW1 where
    kindTag = TagFW1
    getTP _ = tpFW1
    setTP _ x tp = tp { tpFW1 = x }

instance IsKind FW2 where
    kindTag = TagFW2
    getTP _ = tpFW2
    setTP _ x tp = tp { tpFW2 = x }

instance IsKind Env where
    kindTag = TagEnv
    getTP _ = tpEnv
    setTP _ x tp = tp { tpEnv = x }

instance IsKind Phasor where
    kindTag = TagPhasor
    getTP _ = tpPhasor
    setTP _ x tp = tp { tpPhasor = x }

instance IsKind Cos2pi where
    kindTag = TagCos2pi
    getTP _ = tpCos2pi
    setTP _ x tp = tp { tpCos2pi = x }

instance IsKind Lookup where
    kindTag = TagLookup
    getTP _ = tpLookup
    setTP _ x tp = tp { tpLookup = x }

instance IsKind Noise where
    kindTag = TagNoise
    getTP _ = tpNoise
    setTP _ x tp = tp { tpNoise = x }

instance IsKind DW where
    kindTag = TagDW
    getTP _ = tpDW
    setTP _ x tp = tp { tpDW = x }

instance IsKind DTap where
    kindTag = TagDTap
    getTP _ = tpDTap
    setTP _ x tp = tp { tpDTap = x }

instance IsKind VDelay where
    kindTag = TagVDelay
    getTP _ = tpVDelay
    setTP _ x tp = tp { tpVDelay = x }

instance IsKind Biquad where
    kindTag = TagBiquad
    getTP _ = tpBiquad
    setTP _ x tp = tp { tpBiquad = x }

instance IsKind Blit where
    kindTag = TagBlit
    getTP _ = tpBlit
    setTP _ x tp = tp { tpBlit = x }

instance IsKind Reson where
    kindTag = TagReson
    getTP _ = tpReson
    setTP _ x tp = tp { tpReson = x }

instance IsKind Tanh where
    kindTag = TagTanh
    getTP _ = tpTanh
    setTP _ x tp = tp { tpTanh = x }

instance IsKind Moog where
    kindTag = TagMoog
    getTP _ = tpMoog
    setTP _ x tp = tp { tpMoog = x }

instance IsKind Reverb where
    kindTag = TagReverb
    getTP _ = tpReverb
    setTP _ x tp = tp { tpReverb = x }

-- }}}

-- }}}

-- references {{{

-- refs {{{

data Ref :: * -> Kind -> * where
    Ref :: Tag k -> {-# UNPACK #-} !Int -> Ref s k

refToInt :: Ref s k -> Int
refToInt (Ref _ k) = k
{-# INLINE refToInt #-}

-- }}}

-- refmaps {{{

newtype RefMap s a = RefMap { unRefMap :: TagProd (IntMap a) }

refmEmpty :: RefMap s a
refmEmpty = RefMap $ diagTP IM.empty
{-# INLINE refmEmpty #-}

refmLookup :: (IsKind k) => Ref s k -> RefMap s a -> Maybe a
refmLookup (Ref tag k) = IM.lookup k . getTP tag . unRefMap
{-# INLINE refmLookup #-}

refmInsert :: (IsKind k) => Ref s k -> a -> RefMap s a -> RefMap s a
refmInsert (Ref tag k) x = RefMap . modifyTP tag (IM.insert k x) . unRefMap
{-# INLINE refmInsert #-}

refmDelete :: (IsKind k) => Ref s k -> RefMap s a -> RefMap s a
refmDelete (Ref tag k) = RefMap . modifyTP tag (IM.delete k) . unRefMap
{-# INLINE refmDelete #-}

-- }}}

-- refsets {{{

newtype RefSet s = RefSet { unRefSet :: TagProd IntSet }

refsEmpty :: RefSet s
refsEmpty = RefSet $ diagTP IS.empty
{-# INLINE refsEmpty #-}

refsMember :: (IsKind k) => Ref s k -> RefSet s -> Bool
refsMember (Ref tag k) = IS.member k . getTP tag . unRefSet
{-# INLINE refsMember #-}

refsInsert :: (IsKind k) => Ref s k -> RefSet s -> RefSet s
refsInsert (Ref tag k) = RefSet . modifyTP tag (IS.insert k) . unRefSet
{-# INLINE refsInsert #-}

refsDelete :: (IsKind k) => Ref s k -> RefSet s -> RefSet s
refsDelete (Ref tag k) = RefSet . modifyTP tag (IS.delete k) . unRefSet
{-# INLINE refsDelete #-}

refsFor :: (Monad m) => RefSet s -> (forall k. (IsKind k) => Ref s k -> m ()) -> m ()
refsFor rs f = sequence_ . mapTP (\tag -> mapM_ (f . Ref tag) . IS.toList) $ unRefSet rs

-- }}}

-- }}}

-- frames {{{

data Frame s = Frame
    { frmParent :: Maybe (FRef s)
    , frmCount :: {-# UNPACK #-} !Int
    , frmRefs :: RefSet s
    }

newtype FRef s = FRef { unFRef :: Int }

frameRoot :: Frame s
frameRoot = Frame
    { frmParent = Nothing
    , frmCount = 1
    , frmRefs = refsEmpty
    }

frameSub :: FRef s -> Frame s
frameSub up = Frame
    { frmParent = Just up
    , frmCount = 1
    , frmRefs = refsEmpty
    }

frameUp :: Frame s -> Frame s
frameUp frm = frm { frmCount = succ $ frmCount frm }

frameDown :: Frame s -> Maybe (Frame s)
frameDown frm = case frmCount frm of
    1 -> Nothing
    n -> Just $ frm { frmCount = pred n }

frameAdd :: (IsKind k) => Ref s k -> Frame s -> Frame s
frameAdd r frm = frm { frmRefs = refsInsert r $ frmRefs frm }

-- }}}

-- contexts {{{

data Ctx s = Ctx
    { ctxRefs :: RefMap s Reg
    , ctxFrames :: IntMap (Frame s)
    }

ctxInit :: Ctx s
ctxInit = Ctx
    { ctxRefs = refmEmpty
    , ctxFrames = IM.singleton 0 frameRoot
    }

ctxNew :: (IsKind k) => Ref s k -> Reg -> Ctx s -> Ctx s
ctxNew ref reg ctx = ctx
    { ctxRefs = refmInsert ref reg $ ctxRefs ctx
    }

ctxKill :: (IsKind k) => Ref s k -> Ctx s -> Ctx s
ctxKill r ctx = ctx { ctxRefs = refmDelete r $ ctxRefs ctx }

ctxReg :: (IsKind k) => Ref s k -> Ctx s -> Reg
ctxReg r ctx = case refmLookup r $ ctxRefs ctx of
    Just reg -> reg
    Nothing -> error "ctxReg lookup failed"

-- }}}

-- events {{{

newtype Event s a = Event { unEvent :: StateT (Ctx s) Gen a }
  deriving (Functor, Applicative, Monad)

genE :: Gen a -> Event s a
genE = Event . lift

newE :: (IsKind k) => Ref s k -> FRef s -> Event s Reg
newE ref (FRef n) = Event $ do
    reg <- lift allocReg
    modify $ ctxNew ref reg
    modify $ \ctx -> ctx
        { ctxFrames = IM.adjust (frameAdd ref) n $ ctxFrames ctx
        }
    return reg

killE :: (IsKind k) => Ref s k -> Event s ()
killE r = Event $ do
    reg <- gets $ ctxReg r
    lift . emit $ I_blank reg
    lift $ freeReg reg
    modify $ ctxKill r

regE :: (IsKind k) => Ref s k -> Event s Reg
regE = Event . gets . ctxReg

beginE :: FRef s -> FRef s -> Event s ()
beginE (FRef n) up = Event . modify $ \ctx -> ctx
        { ctxFrames = IM.insert n (frameSub up)
                    . IM.adjust frameUp (unFRef up)
                    $ ctxFrames ctx
        }

endE :: FRef s -> Event s ()
endE (FRef n) = Event $ do
    Just frm <- gets $ IM.lookup n . ctxFrames
    case frameDown frm of
        Just frm' -> modify $ \ctx ->
            ctx { ctxFrames = IM.insert n frm' $ ctxFrames ctx }
        Nothing -> do
            modify $ \ctx ->
                ctx { ctxFrames = IM.delete n $ ctxFrames ctx }
            refsFor (frmRefs frm) $ unEvent . killE
            case frmParent frm of
                Just up -> unEvent $ endE up
                Nothing -> return ()

-- }}}

-- event streams {{{

data ES s
    = Stop
    | Yield
        { esTime :: {-# UNPACK #-} !Secs
        , esEvent :: Event s ()
        , esCont :: ES s
        }

instance Monoid (ES s) where
    mempty = Stop
    {-# INLINE mempty #-}
    mappend = esMerge
    {-# INLINE mappend #-}

-- construction {{{

esSingle :: Secs -> Event s () -> ES s
esSingle t e = Yield t e Stop
{-# INLINE esSingle #-}

-- prefers the left stream.
esMerge :: ES s -> ES s -> ES s
esMerge = go
  where
    go Stop b = b
    go a Stop = a
    go a@(Yield tA eA a') b@(Yield tB eB b')
        | tB < tA = Yield tB eB $ go a b'
        | otherwise = Yield tA eA $ go a' b
{-# INLINE esMerge #-}

esAppend :: Event s () -> ES s -> ES s
esAppend x Stop = esSingle 0 x
esAppend x (Yield t e s) = Yield t e $ go t s
  where
    go t Stop = esSingle t x
    go _ (Yield t e s) = Yield t e $ go t s

-- }}}

-- consumption {{{

esGen :: Rational -> Event s () -> ES s -> Gen ()
esGen srHertz end es = evalStateT (go 0 es) ctxInit
  where
    go now Stop = unEvent end
    go now (Yield (Secs tSecs) e es) = do
        let tSamps = floor $ tSecs * srHertz
        when (tSamps > now) . lift . emit . I_advance $ toEnum (tSamps - now)
        unEvent e
        go tSamps es

-- }}}

-- }}}

-- scores {{{

-- types {{{

data Scr s = Scr
    { scrNext :: {-# UNPACK #-} !Int
    , scrHere :: Here s
    }

data Here s = Here
    { hereRate :: {-# UNPACK #-} !Int
    , hereT0 :: {-# UNPACK #-} !Secs
    , hereDT :: {-# UNPACK #-} !Secs
    , hereFrame :: {-# UNPACK #-} !(FRef s)
    }

newtype Score s a = Score { unScore :: Scr s -> (a, Scr s, ES s) }

-- }}}

-- instances {{{

instance Functor (Score s) where
    fmap f x = Score $ \s -> case unScore x s of
        (x', s', es) -> (f x', s', es)
    {-# INLINE fmap #-}

instance Applicative (Score s) where
    pure x = Score $ \s -> (x, s, Stop)
    {-# INLINE pure #-}

    f <*> x = Score $ \s -> case unScore f s of
        (f', s', es) -> case unScore x s' of
            (x', s'', es') -> (f' x', s'', es <> es')
    {-# INLINE (<*>) #-}

instance Monad (Score s) where
    return = pure

    x >>= f = Score $ \s -> case unScore x s of
        (x', s', es) -> case unScore (f x') s' of
            (fx, s'', es') -> (fx, s'', es <> es')
    {-# INLINE (>>=) #-}

-- }}}

-- running {{{

runScore :: Int -> Score s () -> Gen ()
runScore srHertz x = esGen (fromIntegral srHertz) end $ case unScore x scr0 of
    (_, _, es) -> es
  where
    end = endE (FRef 0)
    scr0 = Scr
        { scrNext = 1
        , scrHere = Here
            { hereRate = srHertz
            , hereT0 = 0
            , hereDT = 1
            , hereFrame = FRef 0
            }
        }

-- }}}

-- primitives {{{

freshRef :: Tag k -> Score s (Ref s k)
freshRef tag = Score $ \s ->
    let n = scrNext s
        s' = s { scrNext = succ n }
    in (Ref tag n, s', Stop)

freshFRef :: Score s (FRef s)
freshFRef = Score $ \s ->
    let n = scrNext s
        s' = s { scrNext = succ n }
    in (FRef n, s', Stop)

event :: Event s () -> Score s ()
event e = Score $ \s ->
    let t = hereT0 $ scrHere s
    in ((), s, esSingle t e)



sampleRate :: Score s Int
sampleRate = Score $ \s -> (hereRate $ scrHere s, s, Stop)

toPeriods :: Time -> Score s Rational
toPeriods t = Score $ \s -> let
    { h = scrHere s
    ; sr = fromIntegral $ hereRate h
    ; dt = unSecs $ hereDT h
    ; p = (timeSecs t + timeCells t * dt) * sr
    } in (p, s, Stop)

toPeriods' :: Time -> Score s Double
toPeriods' t = fromRational <$> toPeriods t

toNormFreq :: Freq -> Score s Double
toNormFreq f = Score $ \s -> let
    { h = scrHere s
    ; sr = fromIntegral $ hereRate h
    ; dt = unSecs $ hereDT h
    ; p = (freqHertz f + fromRational (freqPerCell f / dt)) / sr
    } in (p, s, Stop)

toNormFreq' :: Freq -> Score s Double
toNormFreq' = toNormFreq

timeToFreq :: Time -> Score s Freq
timeToFreq t = Score $ \s -> let
    { h = scrHere s
    ; dt = unSecs $ hereDT h
    ; f = recip $ timeSecs t + timeCells t * dt
    } in (hz $ fromRational f, s, Stop)

freqToTime :: Freq -> Score s Time
freqToTime f = Score $ \s -> let
    { h = scrHere s
    ; dt = unSecs $ hereDT h
    ; t = recip $ freqHertz f + fromRational (freqPerCell f / dt)
    } in (sec $ toRational t, s, Stop)



localHere :: (Here s -> Here s) -> Score s a -> Score s a
localHere f x = Score $ \s ->
    let h = f $ scrHere s
        s' = s { scrHere = h }
    in case unScore x s' of
        (x', s'', es) -> (x', s'' { scrHere = scrHere s }, es)

shift :: Time -> Score s a -> Score s a
shift t = localHere $ \h@(Here { hereT0 = t0, hereDT = dt }) ->
    let d = timeSecs t + timeCells t * unSecs dt
    in h { hereT0 = Secs $ unSecs t0 + d }

scale :: Time -> Score s a -> Score s a
scale t = localHere $ \h@(Here { hereDT = dt }) ->
    let dt' = timeSecs t + timeCells t * unSecs dt
    in h { hereDT = Secs dt' }

here :: Score s (FRef s)
here = Score $ \s -> (hereFrame $ scrHere s, s, Stop)

frame :: Time -> Score s a -> Score s a
frame t x = do
    up <- here
    fr <- freshFRef
    event $ beginE fr up
    x' <- localHere (\h -> h { hereFrame = fr }) x
    shift t . event $ endE fr
    return x'

-- }}}

-- }}}

-- vim:fdm=marker:
