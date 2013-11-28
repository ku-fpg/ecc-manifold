module Main where

import ECC.Tester

import qualified ECC.BPSK as BPSK

-- code/codename/4
-- moon/min_array/4
-- ["min_array","moon",ns]

main :: IO ()
main = do
  xss <- eccTester (Options ["bpsk/16"] [2,4,6,8,10] 1) (BPSK.code)
  print xss
