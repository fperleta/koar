-- extensions {{{
{-# LANGUAGE DataKinds, GADTs, KindSignatures #-}
-- }}}

-- exports {{{
module Koar.Flow
    where
-- }}}

-- imports {{{
import           Control.Monad
import           Data.Monoid

import           Koar.Common
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
pipe1 f = Pipe $ \(Sink1 m) -> Sink1 `liftM` f m

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
{-# INLINE (<:) #-}

(-:) :: Pipe s ch ch' -> Src s ch -> Src s ch'
Pipe f -: src = Src $ \snk -> do
    tmp <- f snk
    tmp <: src
{-# INLINE (-:) #-}

graft :: Sink s ch' -> Pipe s ch ch' -> Score s (Sink s ch)
graft snk (Pipe f) = f snk
{-# INLINE graft #-}

instance Monoid (Src s ch) where
    mempty = Src $ \snk -> case snk of
        Sink1 m -> touch m
        Sink2 l r -> touch l >> touch r
    mappend s s' = Src $ \snk -> snk <: s >> snk <: s'
    mconcat ss = Src $ \snk -> mapM_ (snk <:) ss

-- }}}

-- sources {{{

-- from {{{

from :: R -> Sink s ch -> Src s ch
from gain src = Src $ \snk -> case (src, snk) of
    (Sink1 m, Sink1 m') -> void $ wireMake m m' gain
    (Sink2 l r, Sink2 l' r') -> void $ wireMake l l' gain >> wireMake r r' gain

-- }}}

-- }}}

-- pipes {{{

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

-- vim:fdm=marker:
