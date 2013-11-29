module Main where

import ECC.Tester
import ECC.Types
import ECC.Utils

import qualified ECC.BPSK as BPSK

-- code/codename/4
-- moon/min_array/4
-- ["min_array","moon",ns]

codes = BPSK.code

main :: IO ()
main = do
  eccTester (Options ["bpsk/16"] [0,8] {- ,2,4,6,8,10]-} 0) codes $ \ eccs ebN0s -> do
          -- here is where we would setup any fancy output
          return $ \  ecc ebN0 bes -> do
                  print (name ecc,ebN0,bitErrorRate ecc bes)
                  return $ sizeBEs bes > 1000 && sumBEs bes > 1000
                      -- make sure all points have at least 100 runs behind them
{-
        est <- nintyFifth gen (message_length ecc) bers
        debug 1 $ "Est. BER = " ++ show (estPoint est)
                     return (ebN0,sizeBEs bers,sumBEs bers,est)
-}
