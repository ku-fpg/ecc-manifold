module Main where

import ECC.Tester
import ECC.Types
import Data.Monoid

import qualified ECC.Code.Unencoded as Unencoded
import qualified ECC.Code.Repetition as Repetition

-- code/codename/4
-- moon/min_array/4
-- ["min_array","moon",ns]

codes :: Code
codes = Unencoded.code <> Repetition.code

-- usage: ./Main 0 2 4 6 8 0  unencoded/1024 repetition/soft/3
main :: IO ()
main = eccMain codes eccPrinter
