module Main where

import Control.Applicative
import Control.Monad
import Control.Concurrent.Async (forConcurrently)
import Control.Exception (finally)
import System.Process (readProcess)

import Options (parse, Options (Options))

-- Opponent team, game service, and exploit program types
type Oppo = String
type Expl = FilePath
type Flag = String

own :: Expl -> Oppo -> IO [Flag]
own e o = lines <$> readProcess e [o] ""

main = do
  (Options e oFile) <- parse
  o <- lines <$> readFile oFile
  fs <- forConcurrently o $ own e
  forM_ (concat fs) $ putStrLn . ("Got flag: " ++)
