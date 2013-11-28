{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
module ECC.Tester where

import ECC.Types
import ECC.Utils
import System.Random.MWC
import Data.Bit
import Control.Monad
--import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.Trans (MonadIO(liftIO))
import Control.Concurrent
import Data.Word
import qualified Data.Vector.Unboxed  as U
import System.Random.MWC.Distributions
import Data.Monoid
import Data.Array.Matrix
import Statistics.Resampling.Bootstrap

data Options = Options
        { codes   :: [String]
        , ebN0s   :: [EbN0]
        , verbose :: Int
        }

eccTester :: Options -> Code -> IO [(String,[(EbN0,Int,Int,Estimate)])]
eccTester opts (Code f) = do
   let debug n msg | n <= verbose opts  = putStrLn msg
                   | otherwise  = return ()
   gen :: GenIO <- withSystemRandom $ asGenIO return
   print "eccTester"
   let eccs = concatMap f (map splitCodename (codes opts))
   sequence
          [ do xs <- sequence
                [ do bers <- testECC (verbose opts) ebN0 ecc
                     debug 1 $ "found " ++ show (sumBEs bers) ++ " bit errors"
                     est <- nintyFifth gen (message_length ecc) bers
                     debug 1 $ "Est. BER = " ++ show (estPoint est)
                     return (ebN0,sizeBEs bers,sumBEs bers,est)
                | ebN0 <- ebN0s opts
                ]
               return (name ecc,xs)
          | ecc <- eccs
          ]

-- Running a *multi* run of an ECC, giving a single ECCReport
testECC :: Int -> EbN0 -> ECC -> IO BEs
testECC verb ebN0 ecc = do
   let debug n msg | n <= verb  = putStrLn msg
                   | otherwise  = return ()

   gen :: GenIO <- withSystemRandom $ asGenIO return
   -- once for now

   let loop !n !bes = do
        debug 1 $ "trying " ++ show n ++ " messages"
        bes1 <- foldM (\ !a !_ -> do bes0 <- runECC verb gen ebN0 ecc
                                     return $ a <> bes0) bes [1..n]
        let errs = sumBEs bes1
        debug 1 $ "found " ++ show errs ++ " so far"
        if errs < 1000 -- Perrins limit
         then loop (n * 2) bes1
         else return bes1
   -- do 1 initial run, so that the total runs are a power of 2
   bes0 <- runECC verb gen ebN0 ecc
   bes <- loop 1 bes0
   return $ bes -- undefined -- [ fromIntegral x / fromIntegral (message_length ecc) | x <- xs ]

splitCodename :: String -> [String]
splitCodename = words . map (\ c -> if c == '/' then ' ' else c)

-- Running a *single* run of an ECC, getting a single bit error count
runECC :: Int -> GenIO -> EbN0 -> ECC -> IO BEs
runECC verb gen ebN0 ecc = do

  let debug n msg | n <= verb  = putStrLn msg
                  | otherwise  = return ()

  debug 3 $ "starting message"
  mess0  <- liftM (fmap mkBit) $ sequence [ uniform gen | _ <- [1..message_length ecc]]
  debug 3 $ "generated message"
  debug 4 $ show mess0

  code0  <- encode ecc mess0
  debug 3 $ "encoded message"
  debug 4 $ show code0

  rx <- txRx_EbN0 ebN0 (rateOf ecc) gen code0
  debug 3 $ "tx/rx'd message"
  debug 4 $ show rx

  (mess1,parity) <- decode ecc rx

  when (length mess0 /= length mess1) $ do
    error $ "before and after codeword different lengths" ++ show (length mess0,length mess1)

  let bitErrorCount = length [ () | (b,a) <- zip mess0 mess1, a /= b ]
  debug 2 $ show bitErrorCount ++ " bit errors in message"

  return $ eventBEs bitErrorCount

-- Adding randomness
txRx_EbN0 :: EbN0 -> Rational -> GenIO -> [Bit] -> IO [Double]
txRx_EbN0 ebnoDB rate gen xs = do
        rs :: [Double]  <- sequence [ standard gen | _ <- [1..length xs] ]
        return $ map (* lc)
               $ zipWith (+) (fmap (* sqrt sigma2) rs)
                             (fmap (* sqrt ec)
                                $ fmap (\ x -> if getBit x then 1 else -1)
                                $ xs)
     where
         sigma2 = ((1/10) ** (ebnoDB/10)) / 2
         ec = fromRational rate
         lc = 2 * sqrt ec / sigma2
