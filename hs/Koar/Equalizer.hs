-- extensions {{{
{-# LANGUAGE DataKinds #-}
-- }}}

-- exports {{{
module Koar.Equalizer
    where
-- }}}

-- imports {{{
import           Control.Monad

import           Koar.Common
import           Koar.LTI
import           Koar.Patch
import           Koar.Score
-- }}}

-- types {{{

data Equalizer s = Equalizer
    { eqBQ :: Ref s Biquad
    , eqStages :: Nat
    }

data EqStage
    = PureGain R
    | BlockDC R
    | ButterLP Freq R Nat
    | LowShelf Freq R
    | HighShelf Freq R

-- }}}

-- make {{{

eqMake :: Ref s P -> Ref s P -> [EqStage] -> Score s (Equalizer s)
eqMake src snk ss = do
    (g, soss) <- dfToSOS . mconcat <$> mapM go ss
    let nstages = fromIntegral $ length soss
    bq <- biquadMake src snk nstages
    biquadGain bq g
    forM_ (zip [0..] soss) $ \(i, (b1, b2, a1, a2)) -> do
        biquadCoeffs bq i b1 b2 a1 a2
    return $ Equalizer bq nstages
  where
    go (PureGain g) = return $ dfGain g
    go (BlockDC r) = return $ dfBlockDC r
    go (ButterLP f g n) = do
        f' <- toNormFreq' f
        return $ dfButterLP f' g n
    go (LowShelf f g) = do
        f' <- toNormFreq' f
        return $ dfLowShelf f' g
    go (HighShelf f g) = do
        f' <- toNormFreq' f
        return $ dfHighShelf f' g

-- }}}

-- vim:fdm=marker:
