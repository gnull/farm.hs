module Main where

import Control.Applicative
import Control.Arrow ((&&&))
import Control.Monad
import Control.Concurrent (threadDelay, MVar, modifyMVar, Chan, getChanContents, writeChan)
import Control.Concurrent.Async (async, waitAnyCancel)
import System.Process (readProcessWithExitCode, readCreateProcessWithExitCode, CreateProcess (..), proc)
import System.Exit (ExitCode)
import Text.Regex.Posix (getAllTextMatches, (=~))

import Options (parse, Options (..))
import Logging (showLog, LogMsg (..))

type Team = String                  -- Opponent team
type Flag = String                  -- Flag

popTeam :: MVar [Team] -> IO Team
popTeam = flip modifyMVar $ return . (tail &&& head)

submitFlag :: Chan LogMsg -> String -> Flag -> IO ()
submitFlag c s f = do
  let p = (proc "sh" ["-xefu"]) { env = Just [("flag", f)] }
  (ret, out, err) <- readCreateProcessWithExitCode p s
  writeChan c $ SubmMsg s ret out err

own :: Options -> Team -> IO [Flag]
own os team = do
  (ret, out, err) <- readProcessWithExitCode (exploit os) (args os ++ [team]) ""
  let result = getAllTextMatches $ (=~ regex os) $ out
  writeChan (logChan os) $ ExplMsg ([exploit os] ++ args os ++ [team]) ret out err result
  return result

thread :: Options -> IO ()
thread os = forever $ do
  team <- popTeam (queue os)
  fs <- own os team
  forM_ fs $ submitFlag (logChan os) (submit os)
  threadDelay $ delay os * 1000000

main = do
  os <- parse
  as <- replicateM (jobs os) $ async $ thread os
  logger <- async $ getChanContents (logChan os) >>= mapM_ (putStrLn . showLog)
  waitAnyCancel $ logger : as
