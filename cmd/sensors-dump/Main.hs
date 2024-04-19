{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Foreign.C (peekCString)
import System.Sensors

#ifdef USE_LM_SENSORS
import System.Sensors.LmSensors (LmSensors)
import System.Sensors.LmSensors.Types
#endif

indent :: Int -> String
indent n = replicate (n * 4) ' '

putStrLnIndented :: Int -> String -> IO ()
putStrLnIndented n s = putStrLn $ indent n ++ s

main :: IO ()
main = do
#ifdef USE_LM_SENSORS
    withSensor @LmSensors 
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
            Left err -> putStrLn $ "Errorinitializing sensors: " ++ show err
            Right _ -> return ()
#else
    putStrLn "No lm-sensors support"
#endif