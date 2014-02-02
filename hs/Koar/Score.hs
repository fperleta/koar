-- extensions {{{
{-# LANGUAGE DataKinds, DeriveFunctor, GADTs, GeneralizedNewtypeDeriving, KindSignatures, RankNTypes #-}
-- }}}

-- exports {{{
module Koar.Score

    -- imports
    ( module Control.Applicative
    , module Control.Monad
    , module Koar.Patchctl

    -- time
    , Secs(..)
    , Units(..)

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

-- time {{{

newtype Secs = Secs { unSecs :: Rational }
  deriving (Eq, Ord, Enum, Num, Fractional, Show)

newtype Units = Units { unUnits :: Rational }
  deriving (Eq, Ord, Enum, Num, Fractional, Show)

-- }}}

-- resource kinds {{{

data Kind
    = P -- passive nodes

data Tag :: Kind -> * where
    TagP :: Tag P

-- }}}

-- refs {{{

data Ref :: * -> Kind -> * where
    Rp :: Int -> Ref s P

refFromInt :: Tag k -> Int -> Ref s k
refFromInt tag n = case tag of
    TagP -> Rp n

refToInt :: Ref s k -> Int
refToInt r = case r of
    Rp k -> k

-- }}}

-- refmaps {{{

data RefMap s a = RefMap
    { refmP :: IntMap a
    }

refmEmpty :: RefMap s a
refmEmpty = RefMap IM.empty

refmInsert :: Ref s k -> a -> RefMap s a -> RefMap s a
refmInsert r x rm = case r of
    Rp k -> rm { refmP = IM.insert k x $ refmP rm }

refmLookup :: Ref s k -> RefMap s a -> Maybe a
refmLookup r rm = case r of
    Rp k -> IM.lookup k (refmP rm)

refmDelete :: Ref s k -> RefMap s a -> RefMap s a
refmDelete r rm = case r of
    Rp k -> rm { refmP = IM.delete k $ refmP rm }

-- }}}

-- refsets {{{

data RefSet s = RefSet
    { refsP :: IntSet
    }

refsEmpty :: RefSet s
refsEmpty = RefSet IS.empty

refsInsert :: Ref s k -> RefSet s -> RefSet s
refsInsert r rs = case r of
    Rp k -> rs { refsP = IS.insert k $ refsP rs }

refsMember :: Ref s k -> RefSet s -> Bool
refsMember r rs = case r of
    Rp k -> IS.member k $ refsP rs

refsFor :: (Monad m) => RefSet s -> (forall k. Ref s k -> m ()) -> m ()
refsFor rs f = do
    mapM_ (f . Rp) . IS.toList $ refsP rs

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

frameAdd :: Ref s k -> Frame s -> Frame s
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

ctxNew :: Ref s k -> Reg -> Ctx s -> Ctx s
ctxNew ref reg ctx = ctx
    { ctxRefs = refmInsert ref reg $ ctxRefs ctx
    }

ctxKill :: Ref s k -> Ctx s -> Ctx s
ctxKill r ctx = ctx { ctxRefs = refmDelete r $ ctxRefs ctx }

ctxReg :: Ref s k -> Ctx s -> Reg
ctxReg r ctx = case refmLookup r $ ctxRefs ctx of
    Just reg -> reg
    Nothing -> error "ctxReg lookup failed"

-- }}}

-- events {{{

newtype Event s a = Event { unEvent :: StateT (Ctx s) Gen a }
  deriving (Functor, Applicative, Monad)

genE :: Gen a -> Event s a
genE = Event . lift

newE :: Ref s k -> FRef s -> Event s Reg
newE ref (FRef n) = Event $ do
    reg <- lift allocReg
    modify $ ctxNew ref reg
    modify $ \ctx -> ctx
        { ctxFrames = IM.adjust (frameAdd ref) n $ ctxFrames ctx
        }
    return reg

killE :: Ref s k -> Event s ()
killE r = Event $ do
    reg <- gets $ ctxReg r
    lift . emit $ I_blank reg
    lift $ freeReg reg
    modify $ ctxKill r

regE :: Ref s k -> Event s Reg
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
    { hereT0 :: {-# UNPACK #-} !Secs
    , hereDT :: {-# UNPACK #-} !Secs
    , hereFrame :: {-# UNPACK #-} !(FRef s)
    }

newtype Score s a = Score { unScore :: Scr s -> (a, Scr s, ES s) }

-- }}}

-- instances {{{

instance Functor (Score s) where
    fmap f x = Score $ \s -> case unScore x s of
        (x', s', es) -> (f x', s', es)

instance Applicative (Score s) where
    pure x = Score $ \s -> (x, s, Stop)

    f <*> x = Score $ \s -> case unScore f s of
        (f', s', es) -> case unScore x s' of
            (x', s'', es') -> (f' x', s'', es <> es')

instance Monad (Score s) where
    return = pure

    x >>= f = Score $ \s -> case unScore x s of
        (x', s', es) -> case unScore (f x') s' of
            (fx, s'', es') -> (fx, s'', es <> es')

-- }}}

-- running {{{

runScore :: Rational -> Score s () -> Gen ()
runScore srHertz x = esGen srHertz end $ case unScore x scr0 of
    (_, _, es) -> es
  where
    end = endE (FRef 0)
    scr0 = Scr
        { scrNext = 1
        , scrHere = Here
            { hereT0 = 0
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
    in case tag of
        TagP -> (Rp n, s', Stop)

freshFRef :: Score s (FRef s)
freshFRef = Score $ \s ->
    let n = scrNext s
        s' = s { scrNext = succ n }
    in (FRef n, s', Stop)

localHere :: (Here s -> Here s) -> Score s a -> Score s a
localHere f x = Score $ \s ->
    let h = f $ scrHere s
        s' = s { scrHere = h }
    in case unScore x s' of
        (x', s'', es) -> (x', s'' { scrHere = scrHere s }, es)

shift :: Units -> Score s a -> Score s a
shift t = localHere $ \h@(Here { hereT0 = t0, hereDT = dt }) ->
    h { hereT0 = Secs $ unSecs t0 + unUnits t * unSecs dt }

scale :: Units -> Score s a -> Score s a
scale x = localHere $ \h@(Here { hereDT = dt }) ->
    h { hereDT = Secs $ unUnits x * unSecs dt }

event :: Event s () -> Score s ()
event e = Score $ \s ->
    let t = hereT0 $ scrHere s
    in ((), s, esSingle t e)

here :: Score s (FRef s)
here = Score $ \s -> (hereFrame $ scrHere s, s, Stop)

frame :: Units -> Score s a -> Score s a
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
