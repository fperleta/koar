-- extensions {{{
{-# LANGUAGE DataKinds #-}
-- }}}

-- exports {{{
module Test
    where
-- }}}

-- imports {{{
import           Koar.Common
import           Koar.Equalizer
import           Koar.Patch
import           Koar.Patchctl (runInstrs)
import           Koar.Score
-- }}}

main :: IO ()
--main = runInstrs "127.0.0.1:20350" . runGen $ runScore 48000 score
main = runSlave . runGen $ runScore 48000 440 score

score :: Score s ()
score = scale (sec $ 60 / 163) $ do

    outL <- makeSum
    outR <- makeSum
    fw <- fwriter2Make "test.aif" outL outR

    master <- makeSum
    touch master

    final <- makeSum
    eqMake master final
        [ BlockDC 0.999999
        , LowShelf (hz 20) (dBn 3)
        , HighShelf (hz 7800) (dB 24)
        , ButterLP (hz 8000) 1 10
        , Peaking (hz 6000) (dB 6) 8
        ]
    wireMake final outL 1
    wireMake final outR 1

    shift 32 . frame 16 $ do
        freq <- makeSum
        blit <- blitMake freq master
        blitBipolar blit

        fenv <- envMake freq =<< toNormFreq' (hz 1)
        envLin fenv 0.001 4
        shift 4 $ envXdec fenv 0 4
        shift 8 $ envLin fenv 0.1 8

    shift 48 . frame 16 $ do
        tmp <- makeSum
        fsig <- makeSum
        qsig <- makeSum

        rs <- resonMake tmp fsig qsig final
        resonMode rs ConstPeakGain 1

        do
            fenv <- envMake fsig =<< toNormFreq' (hz $ 4 * 880)
            qenv <- envMake qsig 1

            do f <- toNormFreq' $ hz 440; envXdec fenv f 2
            envLin qenv 50 8

            shift 8 $ do
                do f <- toNormFreq' (hz 880); envXdec fenv f 2
                envLin qenv 100 8
                resonMode rs LowPass2Pole (dBn 3)
                frame 8 $ do
                    out <- makeSum
                    wireMake out fsig 0.01
                    ph <- makeSum
                    cos2piMake ph out
                    wobble <- makeSum
                    phasorMake wobble ph
                    wenv <- envMake wobble =<< toNormFreq' 2
                    return ()

        noiseMake tmp 20350

    shift 64 . frame 16 $ do
        tmp <- makeSum
        fsig <- makeSum
        qsig <- makeSum

        rs <- resonMake tmp fsig qsig final
        resonMode rs LowPass2Pole 1

        do
            fenv <- envMake fsig =<< toNormFreq' (hz $ 4 * 880)
            qenv <- envMake qsig 100

            do f <- toNormFreq' $ hz 110; envXdec fenv f 2
            envLin qenv 20 8

            fsig' <- makeSum; fenv' <- envMake fsig' =<< toNormFreq' (hz 100)

            blit <- blitMake fsig' tmp
            --blitBipolar blit

            return ()

    shift 80 . frame 32 $ do
        tmp <- makeSum; touch tmp
        fsig <- makeSum
        ksig <- makeSum

        moog <- moogMake tmp fsig ksig final

        do
            fenv <- envMake fsig =<< toNormFreq' (hz $ 880)
            kenv <- envMake ksig 0

            --do f <- toNormFreq' . hz $ 4 * 880; envLin fenv f 16
            envLin kenv 3.5 14
            shift 14 $ envXdec kenv 0 2

            fsig' <- makeSum; fenv' <- envMake fsig' =<< toNormFreq' (hz 220)
            --do f <- toNormFreq' . hz $ 440; envXdec fenv' f 16

            frame 16 $ do
                --noiseMake tmp 20350
                blit <- blitMake fsig' tmp
                blitGain blit $ dB 6
                blitBipolar blit
                return ()

            return ()

    echo <- makeSum
    wireMake echo master 0.5
    dw <- dwriterMake echo (sec 4)

    do -- echo feedback loops
        rec1 <- makeSum
        wireMake rec1 echo 0.75
        dtapMake rec1 dw . fromRational $ 2 / 3

        rec2 <- makeSum
        wireMake rec2 echo 0.1
        dtapMake rec2 dw . fromRational $ 1 / 5

        {--}
        rec3 <- makeSum
        wireMake rec3 echo 0.1
        src3 <- makeSum
        dtapMake src3 dw . fromRational $ 1 / 9
        dsig <- makeSum
        vd <- vdelayMake src3 dsig rec3 (sec 1)
        vdelayGains vd (0.5) (-0.1) (-0.9)

        denv <- envMake dsig 120
        envLin denv 140 8
        shift 8 $ envXdec denv 120 2
        shift 12 $ envLin denv 180 2
        shift 16 $ envLin denv 120 2

        dosc <- makeSum
        wireMake dosc dsig 100
        pc <- makeSum
        cos2piMake pc dosc

        bla <- envMake pc =<< toNormFreq' 32
        flip (envLin bla) 16 =<< toNormFreq' 64
        shift 16 $ flip (envXdec bla) 8 =<< toNormFreq' 4
        shift 24 $ flip (envLin bla) 16 =<< toNormFreq' 64
        --}

    wave <- arrayMake 1024 0
    arrayPartial wave 1 1 0
    arrayPartial wave 0.3 2 0.1
    arrayPartial wave 0.5 3 0.15
    arrayPartial wave 0.1 4 0.17
    arrayPartial wave 0.3 5 0.75
    arrayNormalize wave 1

    let basicStep = do
            shift 0 $ chirp master 1 (hz 166) (hz 16) (sec 0.125)
            shift 1 $ do
                chirp master 0.4 (hz 220) (hz 16) (sec 0.15)
                snare master 0.6 (hz $ 2 * 880) (hz 16) (sec 0.15)
            shift 2.5 $ chirp master 0.7 (hz 166) (hz 16) (sec 0.125)
            shift 3 $ do
                chirp master 0.4 (hz 220) (hz 16) (sec 0.15)
                snare master 0.6 (hz $ 4 * 880) (hz 16) (sec 0.15)

    let otherStep = do
            shift 0 $ chirp master 1 (hz 166) (hz 16) (sec 0.125)
            shift 1 $ do
                chirp master 0.4 (hz 220) (hz 16) (sec 0.15)
                snare master 0.6 (hz $ 2 * 880) (hz 16) (sec 0.15)
            shift 1.5 $ chirp master 1 (hz 166) (hz 16) (sec 0.125)
            shift 2.5 $ do
                chirp master 0.4 (hz 220) (hz 16) (sec 0.15)
                snare master 0.6 (hz $ 2 * 880) (hz 16) (sec 0.15)
            shift 3 $ chirp master 0.7 (hz 166) (hz 16) (sec 0.125)

    let fillStep = do
            shift 0.5 $ chirp master 0.3 (hz 166) (hz 16) (msec 120)
            shift 3.5 $ chirp master 0.3 (hz 166) (hz 16) (msec 120)


    let basicBass = do
            shift 0 $ pluck master wave 0.3 (hz 55) 1
            shift 1 $ pluck master wave 0.3 (hz $ 55 * 7/5) 1
            shift 2.5 $ pluck master wave 0.3 (hz $ 55 * 7/5) 1

    let basicHihat = forM_ [0 .. 7] $ \i -> do
            shift (fromRational $ i / 2) $ noise master 0.2 0.125

    {--}
    frame 32 $ do
        shift 0 $ chirp echo 0.5 (hz 440) (hz 880) (msec 50)
        shift 7 $ chirp echo 0.5 (hz 1760) (hz 880) (msec 50)
        shift 8 $ chirp echo 0.1 (hz 3520) (hz 880) (msec 150)
        shift 16 $ chirp echo 0.5 (hz 880) (hz 440) (msec 100)
    --}

    {--}
    frame 32 $ do
        shift 0 $ basicStep -- >> basicBass -- >> basicHihat
        shift 4 $ basicStep >> fillStep -- >> basicBass
        shift 8 $ basicStep -- >> basicBass -- >> basicHihat
        shift 12 $ otherStep -- >> basicBass -- >> basicHihat
        shift 16 $ chirp echo 0.5 (hz 220) (hz 16) (sec 0.15)
        --shift 18 $ chirp echo 0.2 (hz $ 1 * 880) (hz 16) (sec 0.15)
        shift 18 $ snare echo 0.2 (hz $ 2 * 880) (hz 16) (sec 0.15)
        shift 24 $ basicStep >> fillStep
        shift 28 $ otherStep
    --}


chirp :: Ref s P -> Double -> Freq -> Freq -> Time -> Score s ()
chirp out amp f0 f1 dur = frame dur $ do
    fc <- makeSum
    pc <- makeSum
    xx <- makeProd

    phasorMake fc pc
    cos2piMake pc xx
    th <- tanhMake xx out
    tanhGain th amp
    tanhSlope th $ dB 3

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

snare :: Ref s P -> Double -> Freq -> Freq -> Time -> Score s ()
snare out amp f0 f1 dur = frame dur $ do
    maxDelay <- scaleTime 1.1 <$> freqToTime f1

    xx <- makeProd
    wireMake xx out amp

    aenv <- envMake xx 1
    envXdec aenv 0 $ scaleTime (1/4) dur
    shift dur . frame (msec 10) $ envLin aenv 0 (msec 10)

    exc <- makeSum
    touch exc

    dsig <- makeSum
    vd <- vdelayMake exc dsig xx maxDelay
    vdelayGains vd 1 1 (0.9)

    frame (msec 40) $ noiseMake exc 20350 >>= noisePink

    denv <- envMake dsig =<< recip <$> toNormFreq' f0
    do f <- toNormFreq' f1; envXdec denv (recip f) $ scaleTime (1/5) dur

-- vim:fdm=marker:
