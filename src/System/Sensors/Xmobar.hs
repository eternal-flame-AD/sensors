{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module System.Sensors.Xmobar where

import Control.Concurrent.MVar
import Data.List (nubBy)
import System.Sensors
import System.Sensors.LmSensors
import System.Sensors.Monitor
import System.Sensors.NvidiaSMI
import Text.Printf
import Xmobar (Exec (..))

data Config = Config
    { configFragments :: [Fragment]
    , configRate :: Int
    }
    deriving (Show, Read)

data MonitorTargets = MonitorTargets
    { nvidiaSMITargets :: [(Int, MonitorSpec)]
    , lmSensorsTargets :: [(Int, MonitorSpec)]
    }
    deriving (Show, Read)

instance Semigroup MonitorTargets where
    MonitorTargets a b <> MonitorTargets c d = MonitorTargets (a <> c) (b <> d)

instance Monoid MonitorTargets where
    mempty = MonitorTargets [] []

buildMonitorTargets :: Config -> MonitorTargets
buildMonitorTargets config = foldl go (MonitorTargets [] []) $ zip [0 ..] $ configFragments config
  where
    go acc (i, FragmentSensor spec _) = case sensorSpecBackend spec of
        "nvidia-smi" -> acc{nvidiaSMITargets = (i, sensorSpecToMonitorSpec spec) : nvidiaSMITargets acc}
        "lm-sensors" -> acc{lmSensorsTargets = (i, sensorSpecToMonitorSpec spec) : lmSensorsTargets acc}
        _ -> acc
    go acc _ = acc

data Fragment
    = FragmentText String
    | FragmentSensor SensorSpec (Maybe ValueFormat)
    deriving (Show, Read)

data SensorSpec = SensorSpec
    { sensorSpecBackend :: String
    , sensorSpecChip :: String
    , sensorSpecFeature :: String
    , sensorSpecSubFeature :: String
    }
    deriving (Show, Read, Eq)

sensorSpecToMonitorSpec :: SensorSpec -> MonitorSpec
sensorSpecToMonitorSpec spec = MonitorSpec (sensorSpecChip spec) (sensorSpecFeature spec) (sensorSpecSubFeature spec)

data ValueFormat = ValueFormat
    { colorLowValue :: Double
    , colorHighValue :: Double
    , colorLowColor :: String
    , colorHighColor :: String
    , formatStr :: String
    }
    deriving (Show, Read)

newtype MonitorResults = MonitorResults [(MonitorSpec, SensorValue)]
    deriving (Show, Read)

render :: MonitorResults -> [Fragment] -> String
render (MonitorResults results) fragments = concatMap go fragments
  where
    go :: Fragment -> String
    go (FragmentText text) = text
    go (FragmentSensor spec format) = case lookup (sensorSpecToMonitorSpec spec) results of
        Just (DoubleValue value) -> case format of
            Nothing -> show value
            Just (ValueFormat low high lowColor highColor formatStr)
                | value < low -> "<fc=" <> lowColor <> ">" <> (if null formatStr then show value else printf formatStr $ value) <> "</fc>"
                | value > high -> "<fc=" <> highColor <> ">" <> (if null formatStr then show value else printf formatStr $ value) <> "</fc>"
                | otherwise -> (if null formatStr then show value else printf formatStr $ value)
        Just (IntValue value) -> case format of
            Nothing -> show value
            Just (ValueFormat low high lowColor highColor formatStr)
                | fromIntegral value < low -> "<fc=" <> lowColor <> ">" <> (if null formatStr then show value else printf formatStr $ value) <> "</fc>"
                | fromIntegral value > high -> "<fc=" <> highColor <> ">" <> (if null formatStr then show value else printf formatStr $ value) <> "</fc>"
                | otherwise -> (if null formatStr then show value else printf formatStr $ value)
        Just (StringValue value) -> case format of
            Nothing -> value
            Just (ValueFormat _ _ _ _ formatStr) -> if null formatStr then value else printf formatStr value
        Nothing -> "N/A"

instance Semigroup MonitorResults where
    MonitorResults a <> MonitorResults b =
        let
            (MonitorResults a') = MonitorResults a
            (MonitorResults b') = MonitorResults b
         in
            MonitorResults $ nubBy (\(x, _) (y, _) -> x == y) $ b' <> a'

data SensorsState = SensorsState
    { sensorStateResults :: MVar MonitorResults
    }

updateSensorsState :: SensorsState -> [(MonitorSpec, SensorValue)] -> IO ()
updateSensorsState state vals = modifyMVar_ (sensorStateResults state) $ \prev -> do
    let (MonitorResults prevVals) = prev
    return $ MonitorResults $ nubBy (\(a, _) (b, _) -> a == b) $ vals <> prevVals

newtype XmobarSensors = XmobarSensors Config
    deriving (Show, Read)

instance Exec XmobarSensors where
    alias (XmobarSensors _) = "sensors"
    rate (XmobarSensors config) = configRate config
    start (XmobarSensors config) cb = do
        state <- newMVar (MonitorResults []) >>= return . SensorsState
        let targets = buildMonitorTargets config
#ifdef USE_LM_SENSORS
        if (not . null . lmSensorsTargets) targets
            then (setupMonitor @LmSensorsMonitor (map snd $ lmSensorsTargets targets) (configRate config) $ 
                    (\vals -> do
                        updateSensorsState state vals
                        fullResults <- readMVar $ sensorStateResults state
                        cb $ render fullResults $ configFragments config
                    )
            ) >> return ()
            else return ()
#endif
#ifdef USE_NVIDIA_SMI
        if (not . null . nvidiaSMITargets) targets
            then (setupMonitor @NvidiaSMIMonitor (map snd $ nvidiaSMITargets targets) (configRate config) $
                    (\vals -> do
                        updateSensorsState state vals
                        fullResults <- readMVar $ sensorStateResults state
                        cb $ render fullResults $ configFragments config
                    )
            ) >> return ()
            else return ()
#endif
