module Concurrenncy where

import Control.Applicative
import Control.Arrow ((&&&), (***))
import Control.Monad
import Control.Concurrent (MVar, newMVar, modifyMVar, newChan, Chan, getChanContents, writeChan)
import Control.Concurrent.Async (async, waitAnyCancel)

popQueue :: MVar [a] -> IO a
popQueue = flip modifyMVar $ return . (tail &&& head)

workForever :: Int                -- Number of worker jobs to spawn
            -> [w]                -- Work to be done
            -> (w -> IO (l, [f])) -- Worker routine that returns log message and its result
            -> (f -> IO l)        -- Submition routine that saves worker's result
            -> (l -> IO ())       -- Logger routine that processes logging messages
            -> IO ()
workForever jobs todo worker saver logger = do
  log <- newChan
  queue <- newMVar $ cycle todo
  asyncs <- replicateM jobs $ async $ forever $ do
    w <- popQueue queue
    (l, fs) <- worker w
    writeChan log l
    ls <- mapM saver fs
    forM_ ls $ writeChan log
    -- (logger *** mapM (logger =<< saver)) =<< worker =<< popQueue queue
  logJob <- async $ getChanContents log >>= mapM_ logger
  waitAnyCancel $ logJob : asyncs
  return ()
