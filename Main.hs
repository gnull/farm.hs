module Main where

import Control.Applicative
import Control.Monad
import Control.Concurrent.Async (forConcurrently, async, wait, Async, waitAny, asyncThreadId)
import Data.List (delete)
import System.Process (readProcess)

import Options (parse, Options (Options))

-- Opponent team, game service, and exploit program types
type Oppo = String
type Expl = FilePath
type Flag = String

own :: Expl -> Oppo -> IO [Flag]
own e o = lines <$> readProcess e [o] ""

loop :: [Async [Flag]] -> IO ()
loop [] = pure ()
loop as = do
  (a, fs) <- waitAny as
  putStrLn $ (show . asyncThreadId) a ++ ": " ++ unwords fs
  loop $ delete a as

main = do
  (Options e oFile) <- parse
  o <- lines <$> readFile oFile
  as <- mapM (async . own e) o
  loop as
