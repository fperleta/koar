-- extensions {{{
{-# LANGUAGE DataKinds #-}
-- }}}

-- exports {{{
module Test
    where
-- }}}

-- imports {{{
import           Koar.Patch
import           Koar.Patchctl (runInstrs)
import           Koar.Score
-- }}}

main :: IO ()
main = runInstrs "127.0.0.1:20350" . runGen $ runScore 48000 score

score :: Score s ()
score = scale (Sec $ 60 / 163) $ do

    outL <- makeSum
    outR <- makeSum
    fw <- fwriter2Make "test.aif" outL outR

    master <- makeSum
    wireMake master outL 1
    wireMake master outR 1

    shift (Cell 0) $ pluck master 1 (Hz 166) (Hz 16) (Sec 0.125)
    shift (Cell 1) $ pluck master 0.7 (Hz 220) (Hz 133) (Sec 0.15)
    shift (Cell 2) $ pluck master 1 (Hz 166) (Hz 16) (Sec 0.125)
    shift (Cell 3) $ pluck master 0.7 (Hz 166) (Hz 16) (Sec 0.125)

pluck :: Ref s P -> Double -> Freq -> Freq -> Time -> Score s ()
pluck out amp f0 f1 dur = frame dur $ do
    f0' <- realToFrac <$> toNormFreq f0
    f1' <- realToFrac <$> toNormFreq f1
    dur' <- realToFrac <$> toPeriods dur

    fc <- makeSum
    pc <- makeSum
    xx <- makeProd

    phasorMake fc pc
    cos2piMake pc xx
    wireMake xx out amp

    fenv <- envMake fc f0'
    envXdec fenv f1' (dur' / 5)

    aenv <- envMake xx 1
    envXdec aenv 0 (dur' / 5)

-- vim:fdm=marker:
