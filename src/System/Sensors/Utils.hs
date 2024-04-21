{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module: System.Sensors.Utils
Description: Utility functions for the sensors library
License: Apache-2.0
Maintainer: yume@yumechi.jp
Stability: experimental
Portability: POSIX
-}
module System.Sensors.Utils where

import Control.Concurrent
import Control.Exception

-- | Loop an IO action forever, stopping when an exception is thrown n times.
loopRetryNTimes :: Int -> IO () -> IO ()
loopRetryNTimes n f = go n Nothing
 where
  go 0 Nothing = undefined
  go 0 (Just ex) = throwIO ex
  go m _ = do
    res <- try f
    case res of
      Left (ex :: SomeException) -> go (m - 1) (Just ex)
      Right _ -> go n Nothing

-- | Fork an IO action forever, and return a function to cancel it.
forkForeverCancel :: IO () -> IO (IO ())
forkForeverCancel f = do
  cancel <- newEmptyMVar
  let loop = do
        f
        isEmpty <- isEmptyMVar cancel
        if isEmpty
          then loop
          else return ()
  _ <- forkIO loop
  return $ putMVar cancel ()