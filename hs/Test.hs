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
--main = runInstrs "127.0.0.1:20350" . runGen $ runScore 48000 score
main = runSlave . runGen $ runScore 48000 score

score :: Score s ()
score = scale (sec $ 60 / 163) $ do

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
            shift 0 $ bass master 1 (hz 166) (hz 16) (sec 0.125)
            shift 1 $ bass master 1 (hz 220) (hz 16) (sec 0.15)
            shift 2.5 $ bass master 0.7 (hz 166) (hz 16) (sec 0.125)
            shift 3 $ bass master 1 (hz 220) (hz 16) (sec 0.15)

    let basicBass = do
            shift 0 $ pluck master wave 0.3 (hz 55) 1
            shift 1 $ pluck master wave 0.3 (hz $ 55 * 7/5) 1
            shift 2.5 $ pluck master wave 0.3 (hz $ 55 * 7/5) 1

    let basicHihat = forM_ [0 .. 7] $ \i -> do
            shift (fromRational $ i / 2) $ noise master 0.2 0.125

    frame 16 $ do
        --shift (Cell 0.5) $ pluck master wave 0.3 (hz 110) (Cell 3)
        --shift (Cell 1.5) $ pluck master wave 0.3 (hz 220) (Cell 2.5)

        {--
        shift (Cell 0) $ pluck master wave 0.3 (hz $ 55 * 4 / 3) (Cell 2)
        shift (Cell 2) $ pluck master wave 0.3 (hz 55) (Cell 2)
        shift (Cell 3.5) $ pluck master wave 0.3 (hz $ 55 * 7 / 5) (Cell 4)
        shift (Cell 8) $ pluck master wave 0.3 (hz 55) (Cell 2)
        shift (Cell 10) $ pluck master wave 0.3 (hz $ 55 * 5 / 4) (Cell 2)
        shift (Cell 13.5) $ pluck master wave 0.2 (hz $ 55) (Cell 2)
        --}

        shift 0 $ basicStep >> basicBass >> basicHihat
        shift 4 $ basicStep >> basicBass
        shift 8 $ basicStep >> basicBass >> basicHihat
        shift 12 $ basicStep >> basicBass >> basicHihat


bass :: Ref s P -> Double -> Freq -> Freq -> Time -> Score s ()
bass out amp f0 f1 dur = frame dur $ do
    f0' <- toNormFreq' f0
    f1' <- toNormFreq' f1
    dur' <- toPeriods' dur

    fc <- makeSum
    pc <- makeSum
    xx <- makeProd

    phasorMake fc pc
    cos2piMake pc xx
    wireMake xx out amp

    fenv <- envMake fc f0'
    envXdec fenv f1' (dur' / 5)

    let att = sec 0.0025
    att' <- toPeriods' att
    aenv <- envMake xx 0
    envLin aenv 1 att'
    shift att $ envXdec aenv 0 ((dur' - att') / 5)

pluck :: Ref s P -> Ref s Array -> Double -> Freq -> Time -> Score s ()
pluck out wave amp f0 dur = frame dur $ do
    f0' <- toNormFreq' f0
    warble <- toNormFreq' 6
    dur' <- toPeriods' dur

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

    let att = sec 0.01
    let rel = sec 0.5
    att' <- toPeriods' att
    rel' <- toPeriods' rel
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
