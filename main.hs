import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (newChan, readChan, writeChan, Chan)
import Control.Monad

-- Opponent team, game service, and exploit program types
type Oppo = String
type Serv = String
type Expl = FilePath
type Flag = String

-- This function isn't implemented yet
own :: Expl -> Oppo -> Serv -> IO [Flag]
own e o s = pure $ map (++ ("_" ++ s ++ "_" ++ o)) ["flag0", "flag1", "flag2"]

main = do
  let opponents = ["127.0.0.1", "127.0.0.2", "127.0.0.3"]
  let services  = ["1234", "80", "8080"]
  let exploit = "./kek.py"
  let tgs = (,) <$> opponents <*> services

  c <- newChan
  forM tgs $ \(o, s) -> forkIO $ do
      fs <- own exploit o s
      forM_ fs $ writeChan c
  forever $ readChan c >>= (putStrLn . ("Got flag: " ++))
