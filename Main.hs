module Main where

import Control.Applicative
import Control.Arrow ((&&&))
import Control.Monad
import System.Process (readProcessWithExitCode, readCreateProcessWithExitCode, CreateProcess (..), proc)
import Text.Regex.Posix (getAllTextMatches, (=~))

import Options (parse, Options (..))
import Logging (showLog, LogMsg (..))
import Concurrenncy (workForever)

submitFlag :: String -> String -> IO LogMsg
submitFlag s f = do
  let p = (proc "sh" ["-xefu"]) { env = Just [("flag", f)] }
  (ret, out, err) <- readCreateProcessWithExitCode p s
  return $ SubmMsg s ret out err

own :: Options -> String -> IO (LogMsg, [String])
own os team = do
  (ret, out, err) <- readProcessWithExitCode (exploit os) (args os ++ [team]) ""
  let result = getAllTextMatches $ (=~ regex os) $ out
  return (ExplMsg ([exploit os] ++ args os ++ [team]) ret out err result, result)

main = do
  os <- parse
  workForever (jobs os)
              (targets os)
              (own os)
              (submitFlag $ submit os)
              (putStrLn . showLog)
