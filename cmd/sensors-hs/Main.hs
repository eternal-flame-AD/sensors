{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Foreign.C (peekCString)
import Options.Applicative
import System.Sensors

#ifdef USE_LM_SENSORS
import System.Sensors.LmSensors (LmSensors)
import System.Sensors.LmSensors.Types
#endif

#ifdef USE_NVIDIA_SMI
import System.Sensors.NvidiaSMI (NvidiaSMI, NvidiaOptions(..), waitForReady)
import System.Sensors.NvidiaSMI.Types
import Data.Default (Default(def))
#endif

data Options = Options
    { optCommand :: Command
    }

data Command
    = Dump DumpOptions

data DumpOptions = DumpOptions
    { dumpOptionsBackend :: [String]
    }

dumpOpts :: Parser DumpOptions
dumpOpts =
    DumpOptions
        <$> many
            ( strOption
                ( long "backend"
                    <> short 'b'
                    <> metavar "BACKEND"
                    <> help "Backend to dump"
                )
            )

opts :: Parser Options
opts =
    subparser
        ( command "dump" (info (Options <$> (Dump <$> dumpOpts)) (progDesc "Dump sensor data"))
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