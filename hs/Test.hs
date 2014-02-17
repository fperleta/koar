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
    touch master
    wireMake master outL 1
    wireMake master outR 1

    wave <- arrayMake 1024 0
    arrayPartial wave 1 1 0
    arrayPartial wave 0.3 2 0.1
    arrayPartial wave 0.5 3 0.15
    arrayPartial wave 0.1 4 0.17
    arrayPartial wave 0.3 5 0.75
    arrayNormalize wave 1

    let basicStep = do
            shift (Cell 0) $ bass master 1 (Hz 166) (Hz 16) (Sec 0.125)
            shift (Cell 1) $ bass master 1 (Hz 220) (Hz 16) (Sec 0.15)
            shift (Cell 2.5) $ bass master 0.7 (Hz 166) (Hz 16) (Sec 0.125)
            shift (Cell 3) $ bass master 1 (Hz 220) (Hz 16) (Sec 0.15)

    let basicBass = do
            shift (Cell 0) $ pluck master wave 0.3 (Hz 55) (Cell 1)
            shift (Cell 1) $ pluck master wave 0.3 (Hz $ 55 * 7/5) (Cell 1)
            shift (Cell 2.5) $ pluck master wave 0.3 (Hz $ 55 * 7/5) (Cell 1)

    let basicHihat = forM_ [0 .. 7] $ \i -> do
            shift (Cell $ i / 2) $ noise master 0.2 (Cell $ 1 / 8)

    frame (Cell 16) $ do
        --shift (Cell 0.5) $ pluck master wave 0.3 (Hz 110) (Cell 3)
        --shift (Cell 1.5) $ pluck master wave 0.3 (Hz 220) (Cell 2.5)

        {--
        shift (Cell 0) $ pluck master wave 0.3 (Hz $ 55 * 4 / 3) (Cell 2)
        shift (Cell 2) $ pluck master wave 0.3 (Hz 55) (Cell 2)
        shift (Cell 3.5) $ pluck master wave 0.3 (Hz $ 55 * 7 / 5) (Cell 4)
        shift (Cell 8) $ pluck master wave 0.3 (Hz 55) (Cell 2)
        shift (Cell 10) $ pluck master wave 0.3 (Hz $ 55 * 5 / 4) (Cell 2)
        shift (Cell 13.5) $ pluck master wave 0.2 (Hz $ 55) (Cell 2)
        --}

        shift (Cell 0) $ basicStep >> basicBass >> basicHihat
        shift (Cell 4) $ basicStep >> basicBass
        shift (Cell 8) $ basicStep >> basicBass >> basicHihat
        shift (Cell 12) $ basicStep >> basicBass >> basicHihat


bass :: Ref s P -> Double -> Freq -> Freq -> Time -> Score s ()
bass out amp f0 f1 dur = frame dur $ do
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

    let att = Sec 0.0025
    att' <- realToFrac <$> toPeriods att
    aenv <- envMake xx 0
    envLin aenv 1 att'
    shift att $ envXdec aenv 0 ((dur' - att') / 5)

pluck :: Ref s P -> Ref s Array -> Double -> Freq -> Time -> Score s ()
pluck out wave amp f0 dur = frame dur $ do
    f0' <- realToFrac <$> toNormFreq f0
    warble <- realToFrac <$> toNormFreq (PerCell 6)
    dur' <- realToFrac <$> toPeriods dur

    fc <- makeSum
    fc' <- makeSum
    pc <- makeSum
    pc' <- makeSum
    xx <- makeProd

    phasorMake fc pc
    phasorMake fc' pc'
    lookupMake wave pc xx
    cos2piMake pc' xx
    wireMake xx out amp

    fenv <- envMake fc f0'
    fenv' <- envMake fc' warble

    let att = Sec 0.01
    let rel = Sec 0.5
    att' <- realToFrac <$> toPeriods att
    rel' <- realToFrac <$> toPeriods rel
    aenv <- envMake xx 0
    envLin aenv 1 att'
    shift dur . frame rel $ envXdec aenv 0 (rel' / 5)

noise :: Ref s P -> Double -> Time -> Score s ()
noise out amp dur = frame dur $ do
    acc <- makeSum
    n <- noiseMake acc 20350
    noisePink n
    wireMake acc out amp
    return ()

-- vim:fdm=marker:
