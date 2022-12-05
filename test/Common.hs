
{-# LANGUAGE ScopedTypeVariables #-}

module Common where

import           Data.IORef
import           Test.Tasty
import           Test.Tasty.HUnit
import           System.Exit
import           System.Process
import           System.IO
import           Control.Exception
import           Text.Printf
import qualified Test.QuickCheck as QC

type Score = IORef (Int, Int)

runTests :: [Score -> TestTree] -> IO ()
runTests groups = do
  sc <- initScore
  -- defaultMain (tests sc groups) `catch` (\(e :: ExitCode) -> do
  defaultMain (localOption (mkTimeout 5000000) (tests sc groups)) `catch` (\(e :: ExitCode) -> do
    (n, tot) <- readIORef sc
    putStrLn ("OVERALL SCORE = " ++ show n ++ " / "++ show tot)
    throwIO e)

tests :: Score -> [Score -> TestTree] -> TestTree
tests x gs = testGroup "Tests" [ g x | g <- gs ]

--------------------------------------------------------------------------------
-- | Construct a single test case
--------------------------------------------------------------------------------
mkTest' :: (Show b, Eq b) => Score -> (a -> IO b) -> a -> b -> String -> TestTree
--------------------------------------------------------------------------------
mkTest' sc f x r name = scoreTest' sc (f, x, r, 1, name)

--------------------------------------------------------------------------------
scoreTest' :: (Show b, Eq b) => Score -> (a -> IO b, a, b, Int, String) -> TestTree
--------------------------------------------------------------------------------
scoreTest' sc (f, x, expR, points, name) =
  testCase name $ do
    updateTotal sc points
    actR <- f x
    if actR == expR
      then updateCurrent sc points
      else assertFailure "Wrong Result"

updateTotal :: Score -> Int -> IO ()
updateTotal sc n = modifyIORef sc (\(x, y) -> (x, y + n))

updateCurrent :: Score -> Int -> IO ()
updateCurrent sc n = modifyIORef sc (\(x, y) -> (x + n, y))

initScore :: IO Score
initScore = newIORef (0, 0)

--------------------------------------------------------------------------------
scoreProp :: (QC.Testable prop) => Score -> (String, prop, Int) -> TestTree
--------------------------------------------------------------------------------
scoreProp sc (name, prop, n) = scoreTest' sc (act, (), True, n, name)
  where
    act _                    = QC.isSuccess <$> QC.labelledExamplesWithResult args prop
    args                     = QC.stdArgs { QC.chatty = False, QC.maxSuccess = 100 }

--------------------------------------------------------------------------------
-- | Binary (Executable) Tests
--------------------------------------------------------------------------------
data BinCmd = BinCmd
  { bcCmd    :: String
  , bcInF    :: FilePath
  , bcExpF   :: FilePath
  , bcPoints :: Int
  , bcName   :: String
  } deriving (Show)

-- binTest :: Score -> BinCmd -> TestTree
-- binTest sc b  = scoreTest' sc (act, (), True, bcPoints b, bcName b)
--   where act _ = mkBinTest (bcCmd b) (bcInF b) (bcExpF b)

-- mkBinTest execS inF expF = do
--   hSetBuffering stdout LineBuffering -- or even NoBuffering
--   withFile log WriteMode $ \h -> do
--     (_,_,_,ph) <- createProcess $ (shell cmd) {std_out = UseHandle h, std_err = UseHandle h}
--     c          <- waitForProcess ph
--     expected   <- readFile expF
--     actual     <- readFile log
--     return (expected == actual)
--   where
--     log  = inF <.> "log"
--     cmd  = printf "%s < %s" execS inF
