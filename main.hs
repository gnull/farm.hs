import Data.Functor
import Control.Applicative
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (newChan, readChan, writeChan, Chan)
import qualified Control.Concurrent.ThreadManager as TM
import Control.Monad
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

  tm <- TM.make
  c <- newChan
  forM tgs $ \(o, s) -> TM.fork tm $ do
      fs <- own exploit o s
      forM_ fs $ writeChan c
  sequence $ take (length tgs) $ repeat $
    readChan c >>= (putStrLn . ("Got flag: " ++))
  TM.waitForAll tm
