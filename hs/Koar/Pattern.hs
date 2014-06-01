-- extensions {{{
-- {-# LANGUAGE #-}
-- }}}

-- exports {{{
module Koar.Pattern
    where
-- }}}

-- imports {{{
import           Data.Monoid
import           Data.Ratio

import           Koar.Common
import           Koar.Score
-- }}}

-- additive patterns {{{

data AddPat s = AddPat Time (Score s ())

addPat :: AddPat s -> Score s ()
addPat (AddPat _ x) = x

instance Monoid (AddPat s) where
    mempty = AddPat 0 $ pure ()
    mappend (AddPat d f) (AddPat d' f') = AddPat (d + d') $ f >> shift d f'

fill :: Time -> DivPat s -> AddPat s
fill dur (DivPat f) = AddPat dur $ f dur

-- }}}

-- divisive patterns {{{

newtype DivPat s = DivPat (Time -> Score s ())

divPat :: Time -> DivPat s -> Score s ()
divPat dur (DivPat f) = f dur

instance Monoid (DivPat s) where
    mempty = DivPat $ \_ -> pure ()
    mappend (DivPat f) (DivPat g) = DivPat $ \dur -> f dur >> g dur

pause :: DivPat s
pause = mempty

bisect :: Rational -> DivPat s -> DivPat s -> DivPat s
bisect r (DivPat f) (DivPat g) = DivPat $ \dur ->
    let dur' = scaleTime r dur
    in f dur' >> shift dur' (g $ dur - dur')

halves, golden :: DivPat s -> DivPat s -> DivPat s
halves = bisect $ 1 % 2
golden = bisect $ a % b
  where
    fibs = 1 : 1 : zipWith (+) fibs (tail fibs)
    a : b : _ = drop 40 fibs

uniform :: [DivPat s] -> DivPat s
uniform [] = pause
uniform xs = DivPat $ \dur -> do
    let d = recip . fromIntegral $ length xs
    forM_ (zip [0, d ..] xs) $ \(r, DivPat f) ->
        shift (scaleTime r dur) . f $ scaleTime d dur

nonuniform :: [(Rational, DivPat s)] -> DivPat s
nonuniform [] = pause
nonuniform xs = DivPat $ \dur -> do
    let whole = sum $ map fst xs
    let go [] = return ()
        go ((d, DivPat f):ps) = do
            let dur' = scaleTime (d / whole) dur
            f dur'
            shift dur' $ go ps
    go xs

-- }}}

-- vim:fdm=marker:
