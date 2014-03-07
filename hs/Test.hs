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

    echo <- makeSum
    wireMake echo master 1
    dw <- dwriterMake echo (sec 4)

    do -- echo feedback loops
        rec1 <- makeSum
        wireMake rec1 echo 0.75
        dtapMake rec1 dw . fromRational $ 2 / 3

        rec2 <- makeSum
        wireMake rec2 echo 0.1
        dtapMake rec2 dw . fromRational $ 1 / 5

    wave <- arrayMake 1024 0
    arrayPartial wave 1 1 0
    arrayPartial wave 0.3 2 0.1
    arrayPartial wave 0.5 3 0.15
    arrayPartial wave 0.1 4 0.17
    arrayPartial wave 0.3 5 0.75
    arrayNormalize wave 1

    let basicStep = do
            shift 0 $ chirp master 1 (hz 166) (hz 16) (sec 0.125)
            shift 1 $ chirp master 1 (hz 220) (hz 16) (sec 0.15)
            shift 2.5 $ chirp master 0.7 (hz 166) (hz 16) (sec 0.125)
            shift 3 $ chirp master 1 (hz 220) (hz 16) (sec 0.15)

    let basicBass = do
            shift 0 $ pluck master wave 0.3 (hz 55) 1
            shift 1 $ pluck master wave 0.3 (hz $ 55 * 7/5) 1
            shift 2.5 $ pluck master wave 0.3 (hz $ 55 * 7/5) 1

    let basicHihat = forM_ [0 .. 7] $ \i -> do
            shift (fromRational $ i / 2) $ noise master 0.2 0.125

    frame 32 $ do
        shift 0 $ chirp echo 0.5 (hz 440) (hz 880) (msec 50)
        shift 7 $ chirp echo 0.5 (hz 1760) (hz 880) (msec 50)
        shift 8 $ chirp echo 0.1 (hz 3520) (hz 880) (msec 150)
        shift 16 $ chirp echo 0.5 (hz 880) (hz 440) (msec 100)

    frame 32 $ do
        shift 0 $ basicStep -- >> basicBass -- >> basicHihat
        shift 4 $ basicStep -- >> basicBass
        shift 8 $ basicStep -- >> basicBass -- >> basicHihat
        shift 12 $ basicStep -- >> basicBass -- >> basicHihat
        shift 16 $ basicStep
        shift 20 $ basicStep
        shift 24 $ basicStep
        shift 28 $ basicStep


chirp :: Ref s P -> Double -> Freq -> Freq -> Time -> Score s ()
chirp out amp f0 f1 dur = frame dur $ do
    fc <- makeSum
    pc <- makeSum
    xx <- makeProd

    phasorMake fc pc
    cos2piMake pc xx
    wireMake xx out amp

    fenv <- envMake fc =<< toNormFreq' f0
    toNormFreq' f1 >>= \f -> envXdec fenv f $ scaleTime (1/5) dur

    let att = msec 2.5
    aenv <- envMake xx 0
    envLin aenv 1 att
    shift att . envXdec aenv 0 $ scaleTime (1/5) (dur - att)

pluck :: Ref s P -> Ref s Array -> Double -> Freq -> Time -> Score s ()
pluck out wave amp f0 dur = frame dur $ do
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

    fenv <- toNormFreq' f0 >>= envMake fc
    fenv' <- envMake fc' 6

    let att = sec 0.01
    let rel = sec 0.5
    aenv <- envMake xx 0
    envLin aenv 1 att
    shift dur . frame rel $ envXdec aenv 0 $ scaleTime (1/5) rel

noise :: Ref s P -> Double -> Time -> Score s ()
noise out amp dur = frame dur $ do
    acc <- makeSum
    n <- noiseMake acc 20350
    noisePink n
    wireMake acc out amp
    return ()

-- vim:fdm=marker:
