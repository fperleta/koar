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

-- }}}

-- vim:fdm=marker:
