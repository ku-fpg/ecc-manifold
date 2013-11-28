module Main where

import ECC.Tester

import qualified ECC.BPSK as BPSK

-- code/codename/4
-- moon/min_array/4
-- ["min_array","moon",ns]

main :: IO ()
main = eccTester (Options ["bpsk/16"] 0) (BPSK.code)
