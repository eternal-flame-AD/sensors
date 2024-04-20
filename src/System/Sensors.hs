{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : System.Sensors
-- Description : An abstraction for a sensor data source.
-- License     : Apache-2.0
-- Maintainer  : yume@yumechi.jp
-- Stability   : experimental
-- Portability : portable
module System.Sensors
  ( Sensors (..),
    SensorValue (..),
  )
where

import Control.Monad.IO.Class (MonadIO)

-- | A sensor value.
--    The value can be an integer, a double, or a string.
data SensorValue
  = IntValue Int
  | DoubleValue Double
  | StringValue String
  deriving (Eq, Show)

-- | An abstraction for a sensor data source.
--    The data source contains a list of chips, each chip has a list of features, and each feature has a list of subfeatures.
class
  (MonadIO m) =>
  Sensors a opt chip feat subfeat err m
    | a -> opt,
      a -> chip,
      a -> feat,
      a -> subfeat,
      a -> err
  where
  -- | Initialize the sensor data source.
  sensorsInit :: opt -> m (Either err a)

  -- | Destroy the sensor data source.
  sensorsDestroy :: a -> m ()

  -- | Run an action with the sensor data source.
  withSensor :: opt -> (a -> m b) -> m (Either err b)
  withSensor opt f = do
    sensorsInit opt >>= \case
      Left err -> return $ Left err
      Right sensors -> Right <$> f sensors

  -- | Get the list of chips.
  getChips :: a -> m (Either err [chip])

  -- | Get the list of features for a chip.
  getFeatures :: a -> chip -> m (Either err [feat])

  -- | Get the list of subfeatures for a feature.
  getSubFeatures :: a -> chip -> feat -> m (Either err [subfeat])

  -- | Get the value of a subfeature.
  getSubFeatureValue :: a -> chip -> feat -> subfeat -> m (Either err SensorValue)

  {-# MINIMAL sensorsInit, sensorsDestroy, getChips, getFeatures, getSubFeatures, getSubFeatureValue #-}
