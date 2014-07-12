module Ch24.MVarExample where

import Control.Concurrent
import Control.Concurrent.MVar

communicate = do
    m <- newEmptyMVar
    forkIO $ do
        v <- takeMVar m
        putStrLn ("received " ++ show v)
    putStrLn "sending ..."
    putMVar m "wake up!"
