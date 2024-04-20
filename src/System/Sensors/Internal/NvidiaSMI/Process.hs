{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module System.Sensors.Internal.NvidiaSMI.Process where

import Control.Concurrent (MVar, forkIO, isEmptyMVar, newEmptyMVar, putMVar, swapMVar)
import Control.Exception
import Control.Exception (IOException)
import Data.List (intercalate)
import GHC.IO.Handle
import System.Exit (ExitCode)
import System.Process
import System.Sensors.Internal.NvidiaSMI.Parser (readNvidiaCsvLine)
import System.Sensors.NvidiaSMI.Types
import System.Sensors.NvidiaSMI.Types (NvidiaSmiQueryResult (NvidiaSmiQueryResult))
import Text.ParserCombinators.ReadP
import Text.ParserCombinators.ReadP (eof)

data NvidiaSmiProcess = NvidiaSmiProcess
  { processHandle :: ProcessHandle,
    processStderr :: Maybe Handle,
    processFormatHeader :: [String],
    processLastOutput :: MVar NvidiaSmiQueryResult
  }

waitForExit :: NvidiaSmiProcess -> IO ExitCode
waitForExit = waitForProcess . processHandle

readProcessStdout :: [String] -> MVar NvidiaSmiQueryResult -> Handle -> IO ()
readProcessStdout headers mv h = do
  res <- catch @IOException (Right <$> hGetLine h) (\_ -> return $ Left ())
  case res of
    Left () -> return ()
    Right line -> do
      lineFields <- case readP_to_S (readNvidiaCsvLine <* eof) line of
        [(x, "")] -> return x
        _ -> throwIO $ userError "Failed to parse line"
      isEmpty <- isEmptyMVar mv
      if isEmpty
        then putMVar mv $ NvidiaSmiQueryResult headers lineFields
        else
          (swapMVar mv $ NvidiaSmiQueryResult headers lineFields)
            >> return ()

      readProcessStdout headers mv h

killProcess :: NvidiaSmiProcess -> IO ()
killProcess = terminateProcess . processHandle

spawnProcess :: ProcessStartArgs -> IO NvidiaSmiProcess
spawnProcess args = do
  let cmd = "nvidia-smi"
  let args' =
        [ "--query-gpu=" ++ intercalate "," (queryGpu args),
          "--format=" ++ processFormat args,
          "--id=" ++ processStartGpuId args
        ]
  let args'' = case processStartLoopMs args of
        Just loopMs -> ("--loop-ms=" ++ show loopMs) : args'
        Nothing -> args'
  (_, Just stdout, stderr, h) <-
    createProcess
      (proc cmd args'')
        { std_in = NoStream,
          std_out = CreatePipe,
          std_err = if captureStderr args then CreatePipe else Inherit
        }
  headerS <- hGetLine stdout
  header <- case readP_to_S (readNvidiaCsvLine <* eof) headerS of
    [(x, "")] -> return x
    _ -> throwIO $ userError "Failed to parse header"
  mv <- newEmptyMVar
  _ <- forkIO $ readProcessStdout header mv stdout
  return $
    NvidiaSmiProcess
      { processHandle = h,
        processStderr = stderr,
        processFormatHeader = header,
        processLastOutput = mv
      }

spawnListGpusProcess :: IO (Handle, ProcessHandle)
spawnListGpusProcess = do
  (_, Just stdout, _, h) <-
    createProcess
      (proc "nvidia-smi" ["--list-gpus"])
        { std_in = NoStream,
          std_out = CreatePipe,
          std_err = Inherit
        }
  return (stdout, h)

spawnQueryGpuHelpProcess :: IO (Handle, ProcessHandle)
spawnQueryGpuHelpProcess = do
  (_, Just stdout, _, h) <-
    createProcess
      (proc "nvidia-smi" ["--help-query-gpu"])
        { std_in = NoStream,
          std_out = CreatePipe,
          std_err = Inherit
        }
  return (stdout, h)
