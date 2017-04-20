module Main where

import Control.Applicative
import Control.Monad
import Control.Concurrent.Async (forConcurrently)
import Control.Exception (finally)
import System.Process (readProcess)

-- Opponent team, game service, and exploit program types
type Oppo = String
type Serv = String
type Expl = FilePath
type Flag = String

own :: Expl -> Oppo -> Serv -> IO [Flag]
own e o s = lines <$> readProcess e [o, s] ""

main = do
  let opponents = ["127.0.0.1", "127.0.0.2", "127.0.0.3"]
  let services  = ["1234", "80", "8080"]
  let exploit = "echo"
  let tgs = (,) <$> opponents <*> services

  fs <- forConcurrently tgs $ uncurry $ own exploit
  forM_ (concat fs) $ putStrLn . ("Got flag: " ++)
