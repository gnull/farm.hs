module Main where

import Control.Applicative
import Control.Arrow ((&&&))
import Control.Monad
import Control.Concurrent (threadDelay, MVar, newMVar, modifyMVar)
import Control.Concurrent.Async (async, waitAnyCancel)
import System.Process (readProcessWithExitCode, readCreateProcessWithExitCode, CreateProcess (..), proc)
import System.Exit (ExitCode)
import Text.Regex.Posix (getAllTextMatches, (=~))

import Options (parse, Options (Options))

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

submit :: String -> Flag -> IO ()
submit s f = do
  let p = (proc "sh" ["-xefu"]) { env = Just [("flag", f)] }
  ret <- readCreateProcessWithExitCode p s
  putStrLn $ pprintRet s ret []

own :: Expl -> Flagre -> Team -> IO [Flag]
own (e, as) r o = do
  (ret, out, err) <- readProcessWithExitCode e (as ++ [o]) ""
  let result = getAllTextMatches $ (=~ r) $ out
  putStrLn $ pprintRet ([e] ++ as ++ [o]) (ret, out, err) result
  return result

thread :: Int -> String -> Expl -> Flagre -> TeamQueue -> IO ()
thread d s e r q = forever $ do
  o <- popTeam q
  fs <- own e r o
  forM_ fs $ submit s
  threadDelay $ d * 1000000

main = do
  (Options e as oFile sub js' delay reg) <- parse
  (o, js) <- (cycle &&& min js' . length) <$> lines <$> readFile oFile
  m <- newMVar o
  as <- replicateM js $ async $ thread delay sub (e, as) reg m
  waitAnyCancel as
