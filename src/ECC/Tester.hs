{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
module ECC.Tester (eccMain, eccPrinter) where

import ECC.Types
import System.Random.MWC
import Control.Monad
--import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.Trans (MonadIO(liftIO))
import Control.Concurrent
import Data.Word
import qualified Data.Vector.Unboxed  as U
import System.Random.MWC.Distributions
import Data.Monoid
import Statistics.Resampling.Bootstrap
import System.IO
import Data.Char
import System.Environment
import ECC.Estimate
import GHC.Conc
import Control.Concurrent.ParallelIO
import Numeric
import Data.Time.Clock

data Options = Options
        { codenames :: [String]
        , ebN0s     :: [EbN0]
        , verbose   :: Int
        , enough    :: Enough
        } deriving Show


data Enough = BitErrorCount Int
            | MessageCount Int
        deriving Show

data TestRun = TestRun 
       { test_packet_count   :: !Int -- how many packets
       , test_encode_time    :: !Float
       , test_decode_time    :: !Float
       , test_ber            :: !BEs
       }
      deriving Show


instance Monoid TestRun where
    mempty = TestRun 0 0 0 mempty
    TestRun p1 en1 de1 ber1 `mappend` TestRun p2 en2 de2 ber2 
        = TestRun (p1 + p2) (en1 + en2) (de1 + de2) (ber1 `mappend` ber2)
    
-- | Give a 'Code' (set of possible Error Correcting Codes) and a printer, run the tests.
eccMain :: Code -> (Options -> [ECC IO] -> IO (ECC IO -> EbN0 -> TestRun -> IO Bool)) -> IO ()
eccMain code k = do
        args <- getArgs
        if null args
         then error $ "usage: <name> [-v<n>] [-b<n>] [-m<n>] <EbN0_1> <EbN0_2> ... <Code Name> <Code Name>"
                   ++ "\ncodes: " ++ show code
         else eccTester (parseOptions args) code k

parseOptions :: [String] -> Options
parseOptions (('-':'v':ns):rest)
        | all isDigit ns = (parseOptions rest) { verbose = read ns }
parseOptions (('-':'b':ns):rest)
        | all isDigit ns = (parseOptions rest) { enough = BitErrorCount (read ns) }
parseOptions (('-':'m':ns):rest)
        | all isDigit ns = (parseOptions rest) { enough = MessageCount (read ns) }
parseOptions (arg:rest) =
        case reads arg of
          [(ebN0::EbN0,"")] -> opts { ebN0s = ebN0 : ebN0s opts }
          _                 -> opts { codenames = arg : codenames opts }
  where
     opts = parseOptions rest
parseOptions [] = Options { codenames = [], ebN0s = [], verbose = 0, enough = BitErrorCount 1000 }

-- | A basic printer for our tests. Currently, we report on powers of two,
-- and acccept a value if there are at least 1000 bit errors.
eccPrinter :: Options -> [ECC f] -> IO (ECC f -> EbN0 -> TestRun -> IO Bool)
eccPrinter opts eccs = do

   let tab1 = maximum (map length (map name eccs))

   let rjust n xs = take (n - length xs) (cycle " ") ++ xs

   gen :: GenIO <- withSystemRandom $ asGenIO return
          -- here is where we would setup any fancy output

   start <- getCurrentTime

   putStrLn $ "#" ++
              rjust 7    "Time" ++ " " ++
              rjust tab1 "EEC" ++ " " ++
              rjust 5    "EbN0" ++ " " ++
              rjust 8    "Packets" ++ " " ++
              rjust 10   "Encode/s" ++ " " ++
              rjust 10   "Decode/s" ++ " " ++
              rjust 8    "Errors" ++ " " ++
              rjust 8    "BER" ++ " " ++
              ""

   return $ \  ecc ebN0 (TestRun tCount tEn tDe bes) -> do
           est <- if sizeBEs bes <= 2
                  then return Nothing
                  else estimate gen 0.95 (message_length ecc) bes
           let accept = case enough opts of
                          BitErrorCount n -> sumBEs bes >= n
                          MessageCount n  -> sizeBEs bes >= n

           now <- getCurrentTime
           let diff = diffUTCTime now start


           putStrLn $
                    rjust 8 (showFFloat (Just 2) (realToFrac diff) "") ++ " " ++
                    rjust tab1 (name ecc) ++ " " ++
                    rjust 5 (showFFloat (Just 2) ebN0 "") ++ " " ++
                    rjust 8 (show (sizeBEs bes)) ++ " " ++
                    rjust 10 (showFFloat (Just 4) (fromIntegral tCount/tEn) "") ++ " " ++
                    rjust 10 (showFFloat (Just 4) (fromIntegral tCount/tDe) "") ++ " " ++
                    rjust 8 (show (sumBEs bes)) ++ " " ++
                    (case est of
                      Just e -> " " ++ rjust 20 (showEstimate e)
                      Nothing -> " 0.00e-0") ++
                    if accept then "." else ","
           hFlush stdout
           return accept


eccTester :: Options -> Code -> (Options -> [ECC IO] -> IO (ECC IO -> EbN0 -> TestRun -> IO Bool)) -> IO ()
eccTester opts (Code _ f) k = do
   print opts
   let debug n msg | n <= verbose opts  = putStrLn msg
                   | otherwise  = return ()
   gen :: GenIO <- withSystemRandom $ asGenIO return
   eccs <- liftM concat
            $ sequence
            $ map f
            $ map splitCodename
            $ codenames opts

   k2 <- k opts eccs
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
   stopGlobalPool -- TODO: use a local pool instead?

-- Running a *multi* run of an ECC, giving a single ECCReport
testECC :: Int -> EbN0 -> ECC IO -> (ECC IO -> EbN0 -> TestRun -> IO Bool) -> IO ()
testECC verb ebN0 ecc k = do
   let debug n msg | n <= verb  = putStrLn msg
                   | otherwise  = return ()

   gen :: GenIO <- withSystemRandom $ asGenIO return
   -- once for now
   cap <- getNumCapabilities

   let ourpar = sequence -- parallel
   let cap = 1           -- for now

   let loop (n:ns) !bes = do
        debug 1 $ "trying " ++ show n ++ " messages"
        let real_par = min (cap * 16) n
        bess <- ourpar   [ do foldM (\ !a !_ -> do bes0 <- runECC verb gen ebN0 ecc
                                                   return $ a <> bes0) mempty [1..(n `div` real_par)]
                         | _ <- [1..real_par]
                         ]
        let bes1 = mconcat (bes:bess)
        let errs = sumBEs $ test_ber bes1
        debug 1 $ "found " ++ show errs ++ " so far"
        okay <- k ecc ebN0 bes1
        if okay then return () else loop ns bes1

   -- We run with the cap twice, then double each time
   loop (cap : iterate (*2) cap) mempty

splitCodename :: String -> [String]
splitCodename = words . map (\ c -> if c == '/' then ' ' else c)

-- Running a *single* run of an ECC, getting a single bit error count
runECC :: Int -> GenIO -> EbN0 -> ECC IO -> IO TestRun
runECC verb gen ebN0 ecc = do

  let debug n msg | n <= verb  = putStrLn msg
                  | otherwise  = return ()

  debug 3 $ "starting message"
  !mess0  <- liftM U.fromList $ sequence [ uniform gen | _ <- [1..message_length ecc]]
  debug 3 $ "generated message"
  debug 4 $ show mess0

  start_encoding <- getCurrentTime

  !code0  <- encode ecc mess0
  debug 3 $ "encoded message"
  debug 4 $ show code0

  end_encoding <- getCurrentTime

  !rx <- txRx_EbN0 ebN0 (rateOf ecc) gen code0
  debug 3 $ "tx/rx'd message"
  debug 4 $ show rx

  
  start_decoding <- getCurrentTime
  
  !(!mess1,parity) <- decode ecc rx

  debug 3 $ "decoded message"
  debug 4 $ "parity: " ++ show parity
  debug 4 $ show mess1

  when (U.length mess0 /= U.length mess1) $ do
    error $ "before and after codeword different lengths" ++ show (U.length mess0,U.length mess1)

  let !bitErrorCount = length [ () | (b,a) <- U.toList $ U.zip mess0 mess1, a /= b ]
  debug 2 $ show bitErrorCount ++ " bit errors in message"

  end_decoding <- getCurrentTime
  
  return $ TestRun 1 (realToFrac $ diffUTCTime end_encoding start_encoding)
                     (realToFrac $ diffUTCTime end_decoding start_decoding) 
         $ eventBEs bitErrorCount

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
txRx_EbN0 :: EbN0 -> Rate -> GenIO -> U.Vector Bool -> IO (U.Vector Double)
txRx_EbN0 ebnoDB rate gen xs
        | isNaN ebnoDB = return $ U.map soft
                                $ xs
txRx_EbN0 ebnoDB rate gen xs = do
        rs :: U.Vector Double  <- U.fromList <$> sequence [ standard gen | _ <- U.toList xs ]
        return $ U.map (* lc)
               $ U.zipWith (+) (U.map (* sqrt sigma2) rs)
                               (U.map (* sqrt ec)
                                  $ U.map soft
                                  $ xs)
     where
         sigma2 = ((1/10) ** (ebnoDB/10)) / 2
         ec     = fromRational rate
         lc     = 2 * sqrt ec / sigma2

