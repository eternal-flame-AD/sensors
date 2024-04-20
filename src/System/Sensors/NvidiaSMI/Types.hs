module System.Sensors.NvidiaSMI.Types where

import System.Exit (ExitCode)

data ProcessStartArgs = ProcessStartArgs
  { processStartGpuId :: String,
    processStartLoopMs :: Maybe Int,
    queryGpu :: [String],
    processFormat :: String,
    captureStderr :: Bool
  }
  deriving (Show)

data NvidiaSmiQueryResult = NvidiaSmiQueryResult
  { queryResultHeader :: [String],
    queryResult :: [String]
  }
  deriving (Show)

data NvidiaError
  = NoGPUFound
  | WrongChipName
  | CannotSet
  | NotFound
  | NotReady
  | ProcessExited ExitCode
  | ParseError String
  deriving (Show)
