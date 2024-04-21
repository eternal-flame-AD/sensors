{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

{- |
Module: System.Sensors.Monitor
Description: A polling-based monitoring system abstraction for sensors
License: Apache-2.0
Maintainer: yume@yumechi.jp
Stability: experimental
Portability: portable
-}
module System.Sensors.Monitor where

import Data.Aeson
import GHC.Generics
import System.Sensors (SensorValue)

-- A generalized specification of which chip, feature, and subfeature to monitor.
data MonitorSpec = MonitorSpec
    { monitorSpecChip :: String
    , monitorSpecFeature :: String
    , monitorSpecSubFeature :: String
    }
    deriving (Generic, Show, Eq, Read)

defaultMonitorSpecParseOptions :: Options
defaultMonitorSpecParseOptions =
    defaultOptions
        { fieldLabelModifier = \case
            "monitorSpecChip" -> "chip"
            "monitorSpecFeature" -> "feature"
            "monitorSpecSubFeature" -> "sub_feature"
            _ -> error "Unknown field"
        }

instance ToJSON MonitorSpec where
    toJSON = genericToJSON defaultMonitorSpecParseOptions

instance FromJSON MonitorSpec where
    parseJSON = genericParseJSON defaultMonitorSpecParseOptions

{- | A @Monitorable@ is a data source that can be polled periodically.
Usually you would wrap a @Sensor@ in a newtype and implement this typeclass.
-}
class Monitorable s where
    setupMonitor ::
        [MonitorSpec] ->
        Int ->
        ([(MonitorSpec, SensorValue)] -> IO ()) ->
        IO s
    destroyMonitor :: s -> IO ()
