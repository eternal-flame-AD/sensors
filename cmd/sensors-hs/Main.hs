{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main (main) where

import Control.Concurrent
import qualified Data.Aeson as Ae
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Default (Default (def))
import Foreign.C (peekCString)
import Options.Applicative
import System.Sensors
import System.Sensors.Monitor

#ifdef USE_LM_SENSORS
import System.Sensors.LmSensors (LmSensors, LmSensorsMonitor)
import System.Sensors.LmSensors.Types
import GHC.Generics (Generic)
#endif

#ifdef USE_NVIDIA_SMI
import System.Sensors.NvidiaSMI (NvidiaSMI, NvidiaSMIMonitor, NvidiaOptions(..), waitForReady)
#endif

data Options = Options
    { optCommand :: Command
    }

data Command
    = Dump DumpOptions
    | Monitor MonitorOptions

data DumpOptions = DumpOptions
    { dumpOptionsBackend :: [String]
    }

data MonitorOptions = MonitorOptions
    { monitorOptionsInterval :: Int
    , monitorOptionsConfig :: FilePath
    }

dumpOpts :: Parser DumpOptions
dumpOpts =
    DumpOptions
        <$> many
            ( strOption
                ( long "backend"
                    <> short 'b'
                    <> help "Backend to dump"
                )
            )

data MonitorOutput = MonitorOutput
    { monitorOutputSpec :: MonitorSpec
    , monitorOutputValue :: SensorValue
    }
    deriving (Generic, Show)

parseMonitorOutputOptions :: Ae.Options
parseMonitorOutputOptions =
    Ae.defaultOptions
        { Ae.fieldLabelModifier = \case
            "monitorOutputSpec" -> "spec"
            "monitorOutputValue" -> "value"
            _ -> error "Unknown field"
        }

instance Ae.ToJSON MonitorOutput where
    toJSON = Ae.genericToJSON parseMonitorOutputOptions

data MonitorConfig = MonitorConfig
    { monitorConfigMonitors :: MonitorConfigBackends
    }
    deriving (Generic, Show)

parseMonitorConfigOptions :: Ae.Options
parseMonitorConfigOptions =
    Ae.defaultOptions
        { Ae.fieldLabelModifier = \case
            "monitorConfigMonitors" -> "monitors"
            _ -> error "Unknown field"
        }

instance Ae.FromJSON MonitorConfig where
    parseJSON = Ae.genericParseJSON parseMonitorConfigOptions

data MonitorConfigBackends = MonitorConfigBackends
    { monitorConfigBackendLmSensors :: Maybe MonitorSpecs
    , monitorConfigBackendNvidiaSMI :: Maybe MonitorSpecs
    }
    deriving (Generic, Show)

parseMonitorConfigBackendsOptions :: Ae.Options
parseMonitorConfigBackendsOptions =
    Ae.defaultOptions
        { Ae.fieldLabelModifier = \case
            "monitorConfigBackendLmSensors" -> "lm-sensors"
            "monitorConfigBackendNvidiaSMI" -> "nvidia-smi"
            _ -> error "Unknown field"
        }

instance Ae.FromJSON MonitorConfigBackends where
    parseJSON = Ae.genericParseJSON parseMonitorConfigBackendsOptions
data MonitorSpecs = MonitorSpecs
    { monitorSpecs :: [MonitorSpec]
    }
    deriving (Generic, Show)

parseMonitorSpecsOptions :: Ae.Options
parseMonitorSpecsOptions =
    Ae.defaultOptions
        { Ae.fieldLabelModifier = \case
            "monitorSpecs" -> "specs"
            _ -> error "Unknown field"
        }

instance Ae.FromJSON MonitorSpecs where
    parseJSON = Ae.genericParseJSON parseMonitorSpecsOptions

monitorOpts :: Parser MonitorOptions
monitorOpts =
    MonitorOptions
        <$> option
            auto
            ( long "interval"
                <> short 'i'
                <> help "Interval in tenths of a second"
                <> value 10
            )
        <*> option
            str
            ( long "config"
                <> short 'c'
                <> help "Configuration file"
            )

opts :: Parser Options
opts =
    subparser
        ( command "dump" (info (Options <$> (Dump <$> dumpOpts)) (progDesc "Dump sensor data"))
            <> command "monitor" (info (Options <$> (Monitor <$> monitorOpts)) (progDesc "Monitor sensor data"))
        )

indent :: Int -> String
indent n = replicate (n * 4) ' '

putStrLnIndented :: Int -> String -> IO ()
putStrLnIndented n s = putStrLn $ indent n ++ s

dumpLmSensors :: IO ()
dumpLmSensors = do
#ifdef USE_LM_SENSORS
    withSensor @LmSensors ()
        (\sensors -> getChips sensors >>= \case
            Left err -> putStrLn $ "Error getting chips: " ++ show err
            Right chips -> 
                (flip mapM_) chips (\chip -> (peekCString $ chipPrefix $ chip) >>= putStrLnIndented 0 >> (
                    getFeatures sensors chip >>= \case
                        Left err -> putStrLn $ "Error getting features for chip " ++ show chip ++ ": " ++ show err
                        Right features -> 
                            (flip mapM_) features (\feature -> (peekCString $ featureName $ feature) >>= putStrLnIndented 1 >> (
                                getSubFeatures sensors chip feature >>= \case
                                    Left err -> putStrLn $ "Error getting subfeatures: chip=" ++ show chip ++ ", feature=" ++ show feature ++ ": " ++ show err
                                    Right subfeatures -> 
                                        (flip mapM_) subfeatures (\subfeature -> (peekCString $ subfeatureName $ subfeature) >>= putStrLnIndented 2 >> (
                                            do
                                                let decodedFlags = decodeSubFeatureFlags $ sensorSubfeatureFlags subfeature
                                                if hasFlag SensorsModeR decodedFlags
                                                    then getSubFeatureValue sensors chip feature subfeature >>= \case
                                                            Left err -> putStrLnIndented 3 $ 
                                                                "Error getting subfeature value: chip=" ++ show chip ++ ", feature=" ++ show feature ++ ", subfeature=" ++ show subfeature ++ ": " ++ show err
                                                            Right value -> putStrLnIndented 3 $ "Value: " ++ show value
                                                    else putStrLnIndented 3 "Subfeature is not readable"
                                                if hasFlag SensorsModeW decodedFlags
                                                    then putStrLnIndented 3 "Subfeature is writable"
                                                    else putStrLnIndented 3 "Subfeature is not writable"
                                                if hasFlag SensorsComputeMapping decodedFlags
                                                    then putStrLnIndented 3 "Subfeature has a mapping"
                                                    else putStrLnIndented 3 "Subfeature does not have a mapping"
                                         ))
                              ))  
                  ))
        ) >>= \case
            Left err -> putStrLn $ "Error initializing sensors: " ++ show err
            Right _ -> return ()
#else
    putStrLn "No lm-sensors support"
#endif

dumpNvidiaSMI :: IO ()
dumpNvidiaSMI = do
#ifdef USE_NVIDIA_SMI
    withSensor @NvidiaSMI def {
            nvidiaOptionsLoopMs = Nothing
            }
        (\sensors -> waitForReady sensors >> getChips sensors >>= \case
            Left err -> putStrLn $ "Error getting chips: " ++ show err
            Right chips -> 
                (flip mapM_) chips (\chip ->  putStrLnIndented 0 (show chip) >> (
                    getFeatures sensors chip >>= \case
                        Left err -> putStrLn $ "Error getting features for chip " ++ show chip ++ ": " ++ show err
                        Right features -> 
                            (flip mapM_) features (\feature ->  putStrLnIndented 1 (show feature)>> (
                                getSubFeatures sensors chip feature >>= \case
                                    Left err -> putStrLn $ "Error getting subfeatures: chip=" ++ show chip ++ ", feature=" ++ show feature ++ ": " ++ show err
                                    Right subfeatures -> 
                                        (flip mapM_) subfeatures (\subfeature ->  putStrLnIndented 2 (show subfeature) >> (
                                            getSubFeatureValue sensors chip feature subfeature >>= \case
                                                Left err -> putStrLnIndented 3 $ 
                                                    "Error getting subfeature value: chip=" ++ show chip ++ ", feature=" ++ show feature ++ ", subfeature=" ++ show subfeature ++ ": " ++ show err
                                                Right value -> putStrLnIndented 3 $ "Value: " ++ show value
                                         ))
                              ))  
                  ))
        ) >>= \case
            Left err -> putStrLn $ "Error initializing sensors: " ++ show err
            Right _ -> return ()
#else
    putStrLn "No nvidia-smi support"
#endif

main :: IO ()
main = do
    Options{optCommand = cmd} <- execParser $ info (opts <**> helper) fullDesc
    case cmd of
        Dump opts -> do
            let backends = dumpOptionsBackend opts
            case backends of
                [] -> do
                    dumpLmSensors
                    dumpNvidiaSMI
                _ ->
                    mapM_
                        ( \backend -> case backend of
                            "lm-sensors" -> dumpLmSensors
                            "nvidia-smi" -> dumpNvidiaSMI
                            _ -> putStrLn $ "Unknown backend: " ++ backend
                        )
                        (dumpOptionsBackend $ opts)
        Monitor opts -> do
            config <- Ae.eitherDecodeFileStrict' $ monitorOptionsConfig opts
            case config of
                Left err -> putStrLn $ "Error parsing config: " ++ err
                Right MonitorConfig{monitorConfigMonitors = monitors} -> do
                    chan <- newChan
#ifdef USE_LM_SENSORS
                    case monitorConfigBackendLmSensors monitors of
                        Just MonitorSpecs{monitorSpecs = specs} -> setupMonitor @LmSensorsMonitor 
                                specs 
                                (monitorOptionsInterval opts) 
                                (\res -> mapM_
                                    ( \case
                                        (spec, value) -> writeChan chan $ MonitorOutput spec value
                                    )
                                    res
                                ) >> return ()
                        Nothing -> return ()
#endif
#ifdef USE_NVIDIA_SMI
                    case monitorConfigBackendNvidiaSMI monitors of
                        Just MonitorSpecs{monitorSpecs = specs} -> setupMonitor @NvidiaSMIMonitor 
                                specs 
                                (monitorOptionsInterval opts)
                                (\res -> mapM_
                                    ( \case
                                        (spec, value) -> writeChan chan $ MonitorOutput spec value
                                    )
                                    res
                                ) >> return ()

                        Nothing -> return ()
#endif
                    let
                        readLoop :: IO ()
                        readLoop = do
                            info <- readChan chan
                            putStrLn $ BSL.unpack $ Ae.encode info
                            readLoop
                     in
                        readLoop