module Main where

import Control.Applicative
import Control.Monad
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (forConcurrently, async, wait, Async, waitAnyCancel, asyncThreadId)
import Data.List (delete)
import System.Process (readProcess)
import Text.Regex.Posix (getAllTextMatches, (=~))

import Options (parse, Options (Options))

type Oppo = String      -- Opponent team
type Expl = FilePath    -- Exploit program
type Flag = String      -- Flag
type Flagre = String    -- Flag regex

own :: Expl -> [String] -> Flagre -> Oppo -> IO [Flag]
own e as r o = getAllTextMatches <$> (=~ r) <$> readProcess e (as ++ [o]) ""

thread :: Expl -> [String] -> Flagre -> Oppo -> IO ()
thread e as r o = forever $ do
  fs <- own e as r o
  forM_ fs $ putStrLn . (("[" ++ o ++ "] got flag: ") ++)
  threadDelay 20000000

main = do
  (Options e as oFile reg) <- parse
  o <- lines <$> readFile oFile
  as <- forM o $ async . thread e as reg
  waitAnyCancel as
