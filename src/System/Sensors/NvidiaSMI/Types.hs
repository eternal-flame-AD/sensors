{- |
Module: System.Sensors.NvidiaSMI.Types
Description: Type definitions for nvidia-smi backend
License: Apache-2.0
Maintainer: yume@yumechi.jp
Stability: experimental
Portability: portable
-}
module System.Sensors.NvidiaSMI.Types where

import System.Exit (ExitCode)

data ProcessStartArgs = ProcessStartArgs
  { processStartGpuId :: String
  -- ^ The UUID of the GPU to start the process on.
  -- Use "nvidia-smi --list-gpus" or @getAllGpus@ to get the UUID
  , processStartLoopMs :: Maybe Int
  -- ^ The interval in milliseconds to poll the process.
  -- If @Nothing@, the process will run once and no updates will be available.
  , queryGpu :: [String]
  -- ^ The query to run on the GPU.
  -- Use @getAllGpuQueries@ to get all available queries.
  , processFormat :: String
  -- ^ The format to output the query.
  -- Use "csv" for CSV format.
  , captureStderr :: Bool
  -- ^ Whether to capture stderr.
  }
  deriving (Show)

data NvidiaSmiQueryResult = NvidiaSmiQueryResult
  { queryResultHeader :: [String]
  , queryResult :: [String]
  }
  deriving (Show)

-- | Errors returned by the nvidia-smi backend
data NvidiaError
  = NoGPUFound
  | WrongChipName
  | CannotSet
  | NotFound
  | -- | Process has started but we have not received any data yet
    NotReady
  | ProcessExited ExitCode
  | ParseError String
  deriving (Show)
