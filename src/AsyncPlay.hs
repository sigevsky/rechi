{-# LANGUAGE TypeApplications #-}
module AsyncPlay where

import Control.Concurrent.Async
import Control.Concurrent (threadDelay, myThreadId)
import Control.Monad
import Control.Exception (bracket, throwIO)
import UnliftIO.STM


withAsync' :: IO a -> (Async a -> IO b) -> IO b
withAsync' io = bracket (async io) uninterruptibleCancel

wait' :: Async a -> IO a
wait' asc = waitCatch asc >>= \case
    Left e -> throwIO e
    Right a -> pure a

printThread :: String -> IO () 
printThread tag = myThreadId >>= \id -> print $ tag <> " " <> show id

blah = atomically @IO $ newTVar (2 :: Int)

main = do
    withAsync (printThread "spawned" >> threadDelay 10000000 >> pure 42) (wait >=> print)
    printThread "main"
    print "hello"