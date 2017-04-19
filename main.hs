import Data.Functor
import Data.Maybe
import Control.Applicative
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (newChan, readChan, writeChan, Chan)
import qualified Control.Concurrent.ThreadManager as TM
import Control.Exception (finally)
import Control.Monad
import System.Process (readProcess)

-- Opponent team, game service, and exploit program types
type Oppo = String
type Serv = String
type Expl = FilePath
type Flag = String

own :: Expl -> Oppo -> Serv -> IO [Flag]
own e o s = lines <$> readProcess e [o, s] ""

thread :: Chan (Maybe Flag) -> Expl -> Oppo -> Serv -> IO ()
thread c e o s = flip finally (closeNChan c) $ do
    fs <- own e o s
    forM_ fs $ writeNChan c

-- Functions that implement a channel closable by N threads on top of Chan

writeNChan :: Chan (Maybe a) -> a -> IO ()
writeNChan c = writeChan c . Just

closeNChan :: Chan (Maybe a) -> IO ()
closeNChan = flip writeChan Nothing

readNChan :: Chan (Maybe a) -> Int -> IO [a]
readNChan _ 0 = pure []
readNChan c n = do
  v <- readChan c
  case v of
    Nothing -> readNChan c (n - 1)
    Just a  -> (a:) <$> readNChan c n

main = do
  let opponents = ["127.0.0.1", "127.0.0.2", "127.0.0.3"]
  let services  = ["1234", "80", "8080"]
  let exploit = "echo"
  let tgs = (,) <$> opponents <*> services

  tm <- TM.make
  c <- (newChan :: IO (Chan (Maybe String)))
  forM tgs $ TM.fork tm . (uncurry $ thread c exploit)
  fs <- readNChan c (length tgs)
  forM_ fs $ putStrLn . ("Got flag: " ++)
  TM.waitForAll tm
