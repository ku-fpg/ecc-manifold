module Main where

import ECC.Tester
import ECC.Types
import Data.Monoid

import qualified ECC.Code.BPSK as BPSK
import qualified ECC.Code.Repetition as Repetition

-- code/codename/4
-- moon/min_array/4
-- ["min_array","moon",ns]

codes :: Code
codes = BPSK.code <> Repetition.code

-- usage: ./Main 0 2 4 6 8 0  bpsk repetition/3
main :: IO ()
main = eccMain codes eccPrinter
