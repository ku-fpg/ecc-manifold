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
import System.IO

data Options = Options
        { codenames :: [String]
        , ebN0s     :: [EbN0]
        , verbose   :: Int
        }

eccPrinter :: [ECC] -> [EbN0] -> IO (ECC -> EbN0 -> BEs -> IO Bool)
eccPrinter eccs ebN0s =
          -- here is where we would setup any fancy output
          return $ \  ecc ebN0 bes -> do
                  putStr $ show (name ecc,ebN0,sumBEs bes,sizeBEs bes,bitErrorRate ecc bes)
                  let accept = sumBEs bes > 1000
                  putStrLn $ " " ++ if accept then " accepted." else " continuting;"
                  hFlush stdout
                  return accept


-- eccTester :: Options -> Code -> IO [(String,[(EbN0,Int,Int,Estimate)])]
eccTester :: Options -> Code -> ([ECC] -> [EbN0] -> IO (ECC -> EbN0 -> BEs -> IO Bool)) -> IO ()
eccTester opts (Code f) k = do
   let debug n msg | n <= verbose opts  = putStrLn msg
                   | otherwise  = return ()
   gen :: GenIO <- withSystemRandom $ asGenIO return
   print "eccTester"
   let eccs = concatMap f (map splitCodename (codenames opts))
   k2 <- k eccs (ebN0s opts)
   sequence_
          [ sequence_
                [ do testECC (verbose opts) ebN0 ecc k2
{-
                     debug 1 $ "found " ++ show (sumBEs bers) ++ " bit errors"
                     est <- nintyFifth gen (message_length ecc) bers
                     debug 1 $ "Est. BER = " ++ show (estPoint est)
                     return (ebN0,sizeBEs bers,sumBEs bers,est)
-}
                | ebN0 <- ebN0s opts
                ]
          | ecc <- eccs
          ]

-- Running a *multi* run of an ECC, giving a single ECCReport
testECC :: Int -> EbN0 -> ECC -> (ECC -> EbN0 -> BEs -> IO Bool) -> IO ()
testECC verb ebN0 ecc k = do
   let debug n msg | n <= verb  = putStrLn msg
                   | otherwise  = return ()

   gen :: GenIO <- withSystemRandom $ asGenIO return
   -- once for now

   let loop !n !bes = do
        -- first, see if it is good enough
        okay <- k ecc ebN0 bes
        if okay then return () else do
                debug 1 $ "trying " ++ show n ++ " messages"
                bes1 <- foldM (\ !a !_ -> do bes0 <- runECC verb gen ebN0 ecc
                                             return $ a <> bes0) bes [1..n]
                let errs = sumBEs bes1
                debug 1 $ "found " ++ show errs ++ " so far"
                loop (n * 2) bes1

   -- do 1 initial run (so that the total runs are a power of 2)
   bes0 <- runECC verb gen ebN0 ecc
   loop 1 bes0

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

{-

    Sigma2 = max(( 1/10^(EbNoDB(jj)/10) )/2, 1e-9);
    Lc    = 2*sqrt(Ec)/Sigma2;

    for ii=1:NumBlocks(jj)

        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        Bits = 0.5 >= rand(1,k);
        CodedBits = mod(Bits*G,2);

        % Modulate the CodedBits
        s     = sqrt(Ec)*(2*CodedBits - 1);
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        % AWGN channel
        noise  = sqrt(Sigma2)*randn(size(s));
        r(1:n) = s + noise;
-}

-- Adding randomness
txRx_EbN0 :: EbN0 -> Rate -> GenIO -> [Bit] -> IO [Double]
txRx_EbN0 ebnoDB rate gen xs = do
        rs :: [Double]  <- sequence [ standard gen | _ <- xs ]
        return $ zipWith (+) (fmap (* sqrt sigma2) rs)
                             (fmap (* sqrt ec)
                                $ fmap (\ x -> if getBit x then 1 else -1)
                                $ xs)
     where
         sigma2 = ((1/10) ** (ebnoDB/10)) / 2
         ec     = fromRational rate
         lc     = 2 * sqrt ec / sigma2

