module Main where

import Control.Applicative
import Control.Monad
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, waitAnyCancel)
import System.Process (readProcess, readCreateProcessWithExitCode, CreateProcess (..), proc)
import Text.Regex.Posix (getAllTextMatches, (=~))

import Options (parse, Options (Options))

type Team = String                  -- Opponent team
type Expl = (FilePath, [String])    -- Exploit program and its extra arguments
type Flag = String                  -- Flag
type Flagre = String                -- Flag regex

submit :: String -> Flag -> IO ()
submit s f = do
  let p = (proc "sh" ["-xefu"]) { env = Just [("flag", f)] }
  (ret, out, err) <- readCreateProcessWithExitCode p s
  putStrLn $ "shell command " ++ show s ++ " returned " ++ show ret ++ ", stdout = " ++ show out ++ ", stderr = " ++ show err

own :: Expl -> Flagre -> Team -> IO [Flag]
own (e, as) r o = do
  stdout <- readProcess e (as ++ [o]) ""
  let result = getAllTextMatches $ (=~ r) $ stdout
  return result

thread :: String -> Expl -> Flagre -> Team -> IO ()
thread s e r o = forever $ do
  fs <- own e r o
  forM_ fs $ submit s
  threadDelay 20000000

main = do
  (Options e as oFile sub reg) <- parse
  o <- lines <$> readFile oFile
  as <- forM o $ async . thread sub (e, as) reg
  waitAnyCancel as
