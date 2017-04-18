import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (newChan, readChan, writeChan)
import Control.Monad

-- Opponent team, game service, and exploit program types
type Oppo = (String, [String])
type Serv = (String, [String])
type Expl = FilePath
type Flag = String

-- This function isn't implemented yet
own :: Expl -> Oppo -> Serv -> IO [Flag]
own e o s = pure ["flag0", "flag1", "flag2"]

main = do
  let opponents = ["127.0.0.1", "127.0.0.2", "127.0.0.3"]
  let services  = ["1234", "80", "8080"]
  let explot = "./kek.py"
  let tgs = (,) <$> opponents <*> services

  c <- newChan
  forM tgs $ \(o, s) -> forkIO $ writeChan c $ "owning " ++ show (o, s)
  forever $ readChan c >>= putStrLn
