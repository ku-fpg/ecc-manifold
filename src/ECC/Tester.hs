module ECC.Tester where

import ECC.Types

data Options = Options
        { codes :: [String]
        , verbose :: Int
        }

eccTester :: Options -> Code -> IO ()
eccTester opts code = do
        print "eccTester"
        return ()

