{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
module ECC.Tester (eccMain, eccPrinter, eccMerger) where

import ECC.Types
import System.Random.MWC
import Control.Monad
--import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.Trans (MonadIO(liftIO))
import Control.Concurrent
import Data.Word
import qualified Data.Vector as V
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
import qualified Data.UUID as UUID
import System.Directory
import qualified Data.Csv as CSV
import qualified Data.ByteString.Lazy as LBS
import Statistics.Types (Estimate (..), ConfInt(..), CL, confidenceLevel)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.List as List
import Data.Ratio

data Options = Options
        { codenames :: [String]
        , ebN0s     :: [EbN0]
        , verbose   :: Int
        , enough    :: Enough
        , logDir    :: String
        , cmp       :: Bool
        } deriving Show


data Enough = BitErrorCount Int
            | MessageCount Int
        deriving Show

data TestRun = TestRun 
       { test_encode_time    :: !Double  -- in seconds
       , test_decode_time    :: !Double -- in seconds
       , test_ber            :: !BEs
       }
      deriving Show


instance Monoid TestRun where
    mempty = TestRun 0 0 mempty
    TestRun en1 de1 ber1 `mappend` TestRun en2 de2 ber2 
        = TestRun (en1 + en2) (de1 + de2) (ber1 `mappend` ber2)
    
-- | Give a 'Code' (set of possible Error Correcting Codes) and a printer, run the tests.
eccMain :: Code -> (Options -> [String] -> IO (ECC IO -> EbN0 -> TestRun -> IO Bool)) -> IO ()
eccMain code k = do
        args <- getArgs
        if null args
         then error $ "usage: <name> [-v<n>] [-b<n>] [-m<n>] [-l<file or dir/>] [-c] <EbN0_1> <EbN0_2> ... <Code Name> <Code Name>"
                   ++ "\ncodes: " ++ show code
         else eccTester (parseOptions args $ defaultOptions "log/") code k

parseOptions :: [String] -> Options -> Options
parseOptions (('-':'v':ns):rest) o
        | all isDigit ns = (parseOptions rest o) { verbose = read ns }
parseOptions (('-':'b':ns):rest) o
        | all isDigit ns = (parseOptions rest o) { enough = BitErrorCount (read ns) }
parseOptions (('-':'m':ns):rest) o
        | all isDigit ns = (parseOptions rest o) { enough = MessageCount (read ns) }
parseOptions (('-':'l':lg):rest) o
        | otherwise = (parseOptions rest o) { logDir = lg }
parseOptions ("-c":rest) o
        | otherwise       = (parseOptions rest o) { cmp = True }
parseOptions (arg:rest) o =
        case reads arg of
          [(ebN0::EbN0,"")] -> opts { ebN0s = ebN0 : ebN0s opts }
          _                 -> opts { codenames = arg : codenames opts }
  where
     opts = parseOptions rest o
parseOptions [] o = o

defaultOptions :: String -> Options
defaultOptions d = Options { codenames = [], ebN0s = [], verbose = 0, enough = BitErrorCount 1000, logDir = d, cmp = False }

-- | A basic printer for our tests. Currently, we report on powers of two,
-- and accept a value if there are at least 1000 bit errors (say).
-- We also output a log file into a diretory, using a UUID style file.
eccPrinter :: Options -> [String] -> IO (ECC f -> EbN0 -> TestRun -> IO Bool)
eccPrinter opts names = do

   let tab1 = maximum (map length names)

   let rjust n xs = take (n - length xs) (cycle " ") ++ xs

   -- This generator is uses for the bootstrapping, and generating the UUID
   gen :: GenIO <- createSystemRandom
   start <- getCurrentTime

   (w1,w2,w3,w4) <- uniform gen
   let uuid = UUID.fromWords w1 w2 w3 w4
    
   logFileName <- if last (logDir opts) == '/' then do
       createDirectoryIfMissing True $ logDir opts
       return $ logDir opts ++ UUID.toString uuid
     else
       return $ logDir opts 

   writeHeader logFileName
   
   putStrLn $ "#" ++
              rjust 7    "Time" ++ " " ++
              rjust tab1 "ECC" ++ " " ++
              rjust 5    "EbN0" ++ " " ++
              rjust 8    "Packets" ++ " " ++
              rjust 10   "Encode/s" ++ " " ++
              rjust 10   "Decode/s" ++ " " ++
              rjust 8    "Errors" ++ " " ++
              rjust 8    "BER" ++ " " ++
              ""

   return $ \  ecc ebN0 (TestRun tEn tDe bes) -> do
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
                    rjust 10 (showFFloat (Just 4) (fromIntegral (sizeBEs bes)/tEn) "") ++ " " ++
                    rjust 10 (showFFloat (Just 4) (fromIntegral (sizeBEs bes)/tDe) "") ++ " " ++
                    rjust 8 (show (sumBEs bes)) ++ " " ++
                    (case est of
                      Just e -> " " ++ rjust 20 (showEstimate e)
                      Nothing -> " 0.00e-0") ++
                    if accept then "." else ","
           hFlush stdout

           -- Log file, with raw data
           LBS.appendFile logFileName $ CSV.encode [ Result
               ( realToFrac diff :: Double )
               ( name ecc        :: String )
               ( ebN0            :: Double ) 
               ( sizeBEs bes     :: Int ) 
               ( tEn        )
               ( tDe        )
               ( sumBEs bes      :: Int )
               ( estPoint <$> est )
               ( (confIntLDX . estError) <$> est )
               ( (confIntUDX . estError) <$> est )
               ( (confidenceLevel . confIntCL . estError) <$> est )
               ( message_length ecc )
               ( codeword_length ecc )
               ( bes )
               ]
           return accept


-- eccTester runs testECC at different ebN0 levels.
eccTester :: Options -> Code -> (Options -> [String] -> IO (ECC IO -> EbN0 -> TestRun -> IO Bool)) -> IO ()
eccTester opts (Code _ f) k = do
   print opts
   let debug n msg | n <= verbose opts  = putStrLn msg
                   | otherwise  = return ()

   -- This generator is uses for the generation of bits
   gen :: GenIO <- createSystemRandom

   let mg :: (Show a, Show b, Eq b, Monad m) => (a -> m b) -> (a -> m b) -> a -> m b
       mg f1 f2 a = do
           r1 <- f1 a
           r2 <- f2 a
           if r1 == r2 
            then return r1
            else fail $ "merge of two codes failed at runtime" ++ show (a,r1,r2)

   let jn :: Monad m => ECC m -> ECC m -> ECC m
       jn ecc1 ecc2 
            | message_length ecc1 /= message_length ecc2 
             = error "trying to combine two codes with different message lengths"
            | codeword_length ecc1 /= codeword_length ecc2 
            = error "trying to combine two codes with different codeword lengths"
            | otherwise
            = ECC { name = name ecc1 ++ "+" ++ name ecc2
                  , encode = mg (encode ecc1) (encode ecc2)
                  , decode = mg (decode ecc1) (decode ecc2)
                  , message_length = message_length ecc1
                  , codeword_length = codeword_length ecc2
                  }
       
   eccs <- liftM concat
            $ sequence
            $ map f
            $ map splitCodename
            $ codenames opts

   let eccs' = if cmp opts then [foldr1 jn eccs] else eccs

   k2 <- k opts (map name eccs)
   sequence_
          [ sequence_
                [ do testECC (verbose opts) gen ebN0 ecc k2
                | ebN0 <- ebN0s opts
                ]
          | ecc <- eccs'
          ]
   stopGlobalPool -- TODO: use a local pool instead?

-- Running a *multi* run of an ECC, giving a compounted TestRun result to the
-- continuation, and stopping when the continuation returns True.
testECC :: Int -> GenIO -> EbN0 -> ECC IO -> (ECC IO -> EbN0 -> TestRun -> IO Bool) -> IO ()
testECC verb gen ebN0 ecc k = do
   let debug n msg | n <= verb  = putStrLn msg
                   | otherwise  = return ()

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
  debug 4 $ show (Message mess0)

  start_encoding <- getCurrentTime

  !code0  <- encode ecc mess0
  debug 3 $ "encoded message"
  debug 4 $ show (TxCodeword code0)

  end_encoding <- getCurrentTime

  !rx <- txRx_EbN0 ebN0 (rateOf ecc) gen code0
  debug 3 $ "tx/rx'd message"
  debug 4 $ show (RxCodeword rx)

  rxECC verb ecc mess0 (realToFrac $ diffUTCTime end_encoding start_encoding) rx 
  
rxECC :: Int -> ECC IO -> U.Vector Bool -> Double -> U.Vector Double -> IO TestRun
rxECC verb ecc mess0 encoding_time rx = do

  let debug n msg | n <= verb  = putStrLn msg
                  | otherwise  = return ()

  start_decoding <- getCurrentTime
  
  !(!mess1,parity) <- decode ecc rx

  debug 3 $ "decoded message"
  debug 4 $ "parity: " ++ show parity
  debug 4 $ show (Decoded (name ecc) mess1)

  when (U.length mess0 /= U.length mess1) $ do
    error $ "before and after codeword different lengths" ++ show (U.length mess0,U.length mess1)

  let !bitErrorCount = length [ () | (b,a) <- U.toList $ U.zip mess0 mess1, a /= b ]
  debug 2 $ show bitErrorCount ++ " bit errors in message"

  end_decoding <- getCurrentTime
  
  return $ TestRun encoding_time
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
         ec     = fromIntegral (numerator rate) / fromIntegral (denominator rate)
         lc     = 2 * sqrt ec / sigma2



-- eccMerge merges all the input files.
eccMerger :: IO ()
eccMerger = do
    args <- getArgs
    if null args
     then error $ "usage: <name> [-v<n>] [-b<n>] [-m<n>] [-l<file or dir/>] [-c] log/<file> log/<file>"
     else do
       let opts = parseOptions args $ defaultOptions "merge/"
--       print opts   
       -- now we try load the files.
       logs <- sequence 
         [ do bs <- LBS.readFile nm
              case CSV.decode CSV.HasHeader bs of
                Left msg -> fail msg
                Right t  -> return $ V.toList $ (t :: V.Vector Result)
         | nm <- codenames opts
         ]
      
       -- first, pick only the final results. We can not merge partual with final results.
       let sumMe :: Result -> Map String (Map Double Result) -> Map String (Map Double Result) 
           sumMe r m = flip (Map.insert (res_name r)) m $
                   case Map.lookup (res_name r) m of
                     Nothing -> Map.singleton (res_ebN0 r) r
                     Just m' -> flip (Map.insert (res_ebN0 r)) m' $
                       case Map.lookup (res_ebN0 r) m' of
                         Nothing -> r
                         Just r' | res_size r >= res_size r' -> r
                                 | otherwise                 -> r'

       let logs' = map (foldr sumMe Map.empty) logs
       -- next, merge the different runs
       let mergeRes :: Result -> Result -> Result
           mergeRes r1 r2 = r1 
              { timestamp = 0.0
              , res_size = res_size r1 + res_size r2
              , res_tEn  = res_tEn r1 + res_tEn r2
              , res_tDe  = res_tDe r1 + res_tDe r2
              , res_sum  = res_sum r1 + res_sum r2
              , res_ber  = Nothing
              , res_ldx  = Nothing
              , res_udx  = Nothing
              , res_conf = Nothing
              , res_bes  = res_bes r1 <> res_bes r2
              }
       let sumLog = Map.unionsWith (Map.unionWith mergeRes) logs'
--       print sumLog

       -- setup the printer

       -- Options -> [String] -> IO (ECC f -> EbN0 -> TestRun -> IO Bool)
       p <- eccPrinter opts (Map.keys sumLog)

       sequence_ [ do
         sequence_ [ p ecc db testrun
                   | db <- List.sort $ Map.keys m
                   , let r = m Map.! db
                     -- These are both fake, for the purposes of the log printing
                   , let ecc = ECC (res_name r) undefined undefined (res_mesg r) (res_code r)
                   , let testrun = TestRun (res_tEn r) (res_tDe r) (res_bes r)
                   ]
                 | (nm,m) <- Map.toList sumLog
                 ]
       
data Result = Result
 { timestamp  :: Double
 , res_name   :: String
 , res_ebN0   :: Double
 , res_size   :: Int     -- number of packets tested
 , res_tEn    :: Double
 , res_tDe    :: Double
 , res_sum    :: Int     -- total bit errors
 , res_ber    :: Maybe Double
 , res_ldx    :: Maybe Double
 , res_udx    :: Maybe Double
 , res_conf   :: Maybe Double
 , res_mesg   :: Int        -- length of message
 , res_code   :: Int        -- length of codeword
 , res_bes    :: BEs
 } deriving Show

instance CSV.ToRecord Result where
    toRecord (Result a b c d e f g h i j k l m n) = V.fromList [
        CSV.toField a, CSV.toField b, CSV.toField c, CSV.toField d, CSV.toField e, CSV.toField f,
        CSV.toField g, CSV.toField h, CSV.toField i, CSV.toField j, CSV.toField k, CSV.toField l,
        CSV.toField m, CSV.toField (show n)]

instance CSV.FromRecord Result where
    parseRecord v
        | n == 14    = Result        <$> CSV.unsafeIndex v 0
                                     <*> CSV.unsafeIndex v 1
                                     <*> CSV.unsafeIndex v 2
                                     <*> CSV.unsafeIndex v 3
                                     <*> CSV.unsafeIndex v 4
                                     <*> CSV.unsafeIndex v 5
                                     <*> CSV.unsafeIndex v 6
                                     <*> CSV.unsafeIndex v 7
                                     <*> CSV.unsafeIndex v 8
                                     <*> CSV.unsafeIndex v 9
                                     <*> CSV.unsafeIndex v 10
                                     <*> CSV.unsafeIndex v 11
                                     <*> CSV.unsafeIndex v 12
                                     <*> (read <$> CSV.unsafeIndex v 13)
        | otherwise = fail "Fail to read Result"
          where
            n = V.length v

writeHeader :: String -> IO ()
writeHeader logFileName = 
   LBS.writeFile logFileName $ CSV.encode [
               [ "Time"
               , "ECC"
               , "EbN0"
               , "Packets"
               , "Encodes"
               , "Decodes"
               , "Errors"
               , "BER"
               , "LDX"
               , "UDX"
               , "Confidence"
               , "Message"
               , "Codeword"
               , "Internal"
               ]]
