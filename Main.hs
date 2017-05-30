module Main where

import Control.Applicative
import Control.Arrow ((&&&))
import Control.Monad
import Control.Concurrent (threadDelay, MVar, newMVar, modifyMVar, Chan, newChan, getChanContents, writeChan)
import Control.Concurrent.Async (async, waitAnyCancel)
import System.Process (readProcessWithExitCode, readCreateProcessWithExitCode, CreateProcess (..), proc)
import System.Exit (ExitCode)
import Text.Regex.Posix (getAllTextMatches, (=~))

import Options (parse, Options (..))

type Team = String                  -- Opponent team
type Expl = (FilePath, [String])    -- Exploit program and its extra arguments
type Flag = String                  -- Flag
type Flagre = String                -- Flag regex

type TeamQueue = MVar [Team]        -- Infinite queue of teams

popTeam :: TeamQueue -> IO Team
popTeam = flip modifyMVar $ return . (tail &&& head)

pprintRet :: Show a => a -> (ExitCode, String, String) -> [Flag] -> String
pprintRet cmd (ret, out, err) flags = unlines
  $  [show cmd ++ " returned " ++ show ret]
  ++ map ("  stderr: " ++) (lines err)
  ++ map ("  stdout: " ++) (lines out)
  ++ map ("    flag: " ++) flags

submit :: Chan String -> String -> Flag -> IO ()
submit c s f = do
  let p = (proc "sh" ["-xefu"]) { env = Just [("flag", f)] }
  ret <- readCreateProcessWithExitCode p s
  writeChan c $ pprintRet s ret []

own :: Chan String -> Expl -> Flagre -> Team -> IO [Flag]
own c (e, as) r o = do
  (ret, out, err) <- readProcessWithExitCode e (as ++ [o]) ""
  let result = getAllTextMatches $ (=~ r) $ out
  writeChan c $ pprintRet ([e] ++ as ++ [o]) (ret, out, err) result
  return result

thread :: Chan String -> Int -> String -> Expl -> Flagre -> TeamQueue -> IO ()
thread c d s e r q = forever $ do
  o <- popTeam q
  fs <- own c e r o
  forM_ fs $ submit c s
  threadDelay $ d * 1000000

main = do
  os <- parse
  m <- newMVar $ cycle $ targets os
  c <- newChan
  as <- replicateM (jobs os) $ async $ thread c (delay os) (sumbit os) (exploit os, args os) (regex os) m
  logger <- async $ getChanContents c >>= mapM_ putStrLn
  waitAnyCancel $ logger : as
