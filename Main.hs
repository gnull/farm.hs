module Main where

import Control.Applicative
import Control.Monad
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (forConcurrently, async, wait, Async, waitAnyCancel, asyncThreadId)
import Data.List (delete)
import System.Process (readProcess)
import Text.Regex.Posix (getAllTextMatches, (=~))

import Options (parse, Options (Options))
import Web (serverStart)

-- Opponent team, game service, and exploit program types
type Oppo = String
type Expl = FilePath
type Flag = String

own :: Expl -> [String] -> String -> Oppo -> IO [Flag]
own e as r o = getAllTextMatches <$> (=~ r) <$> readProcess e (as ++ [o]) ""

processResult :: [Flag] -> IO ()
processResult = mapM_ $ putStrLn . ("Got flag: " ++)

main = do
  srv <- async serverStart
  (Options e as oFile reg) <- parse
  o <- lines <$> readFile oFile
  as <- forM o $ async . forever . (>> threadDelay 20000000) . (>>= processResult) . own e as reg
  waitAnyCancel $ srv : as
