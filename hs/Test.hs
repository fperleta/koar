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
score = do

    outL <- makeSum
    outR <- makeSum
    fw <- fwriter2Make "test.aif" outL outR

    master <- makeSum
    wireMake master outL 1
    wireMake master outR 1

    pluck master 440 1 1
    shift 0.25 $ pluck master 880 0.7 2

pluck :: Ref s P -> Double -> Double -> Units -> Score s ()
pluck out freq amp dur = frame dur $ do
    sr <- sampleRate
    let f0 = freq / realToFrac sr
    durp <- realToFrac <$> toPeriods (Secs $ unUnits dur)

    fc <- makeSum
    pc <- makeSum
    xx <- makeProd

    phasorMake fc pc
    cos2piMake pc xx
    wireMake xx out amp

    fenv <- envMake fc f0
    envXdec fenv (f0 / 2.0) (durp / 5)

    aenv <- envMake xx 1
    envXdec aenv 0 (durp / 5)

-- vim:fdm=marker:
