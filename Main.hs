{-# LANGUAGE RecordWildCards            #-}

module Main where

import Control.Applicative
import Control.Arrow ((&&&))
import Control.Monad
import Data.Maybe (maybeToList)
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

own :: String -> [String] -> String -> String -> IO (LogMsg, [String])
own exploit args regex team = do
  (ret, out, err) <- readProcessWithExitCode exploit (args ++ [team]) ""
  let result = getAllTextMatches $ (=~ regex) $ out
  return $ flip (,) result $ ExplMsg ([exploit] ++ args ++ [team]) ret out err result

main = do
  Options {..} <- parse
  targetsList <- lines <$> readFile targets
  workForever (minimum $ length targetsList : maybeToList jobs)
              targetsList
              (own exploit args regex)
              (submitFlag $ submit)
              (putStrLn . showLog)
