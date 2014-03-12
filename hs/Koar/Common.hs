-- extensions {{{
-- {-# LANGUAGE #-}
-- }}}

-- exports {{{
module Koar.Common
    where
-- }}}

-- imports {{{
-- import qualified X as Y
-- import           X (Y)
import           Data.Complex
import           Data.Word
import           Text.Printf
-- }}}

-- types {{{

type N = Word
type R = Double
type C = Complex R

-- }}}

-- decibels {{{

dB :: R -> R
dB db = 10 ** (db / 20)

dBn :: R -> R
dBn = dB . negate

dBshow :: R -> String
dBshow x = printf "%.2fdB" $ 20 * logBase 10 x

-- }}}

-- vim:fdm=marker:
