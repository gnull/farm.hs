module Main where

import Control.Applicative
import Control.Arrow ((&&&))
import Control.Monad
import Control.Concurrent (threadDelay, MVar, newMVar, modifyMVar)
import Control.Concurrent.Async (async, waitAnyCancel)
import System.Process (readProcessWithExitCode, readCreateProcessWithExitCode, CreateProcess (..), proc)
import Text.Regex.Posix (getAllTextMatches, (=~))

import Options (parse, Options (Options))

type Team = String                  -- Opponent team
type Expl = (FilePath, [String])    -- Exploit program and its extra arguments
type Flag = String                  -- Flag
type Flagre = String                -- Flag regex

type TeamQueue = MVar [Team]        -- Infinite queue of teams

popTeam :: TeamQueue -> IO Team
popTeam = flip modifyMVar $ return . (tail &&& head)

submit :: String -> Flag -> IO ()
submit s f = do
  let p = (proc "sh" ["-xefu"]) { env = Just [("flag", f)] }
  (ret, out, err) <- readCreateProcessWithExitCode p s
  putStrLn $ "shell command " ++ show s ++ " returned " ++ show ret ++ ", stdout = " ++ show out ++ ", stderr = " ++ show err

own :: Expl -> Flagre -> Team -> IO [Flag]
own (e, as) r o = do
  (ret, out, err) <- readProcessWithExitCode e (as ++ [o]) ""
  putStrLn $ "command " ++ show (e : as ++ [o]) ++ " returned " ++ show ret ++ ", stderr = " ++ show err
  let result = getAllTextMatches $ (=~ r) $ out
  return result

thread :: String -> Expl -> Flagre -> TeamQueue -> IO ()
thread s e r q = forever $ do
  o <- popTeam q
  fs <- own e r o
  forM_ fs $ submit s
  threadDelay 20000000

main = do
  (Options e as oFile sub js' reg) <- parse
  (o, js) <- (cycle &&& min js' . length) <$> lines <$> readFile oFile
  m <- newMVar o
  as <- sequence $ replicate js $ async $ thread sub (e, as) reg m
  waitAnyCancel as
