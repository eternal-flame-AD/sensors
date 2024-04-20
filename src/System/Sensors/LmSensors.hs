{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module: System.Sensors.LmSensors
-- Description: Implementation of the [@Sensors@] typeclass for lm-sensors
-- License: Apache-2.0
-- Maintainer: yume@yumechi.jp
-- Stability: experimental
-- Portability: POSIX
module System.Sensors.LmSensors
  ( LmSensors,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Coerce (coerce)
import Foreign
import Foreign.C (CDouble (CDouble))
import System.Sensors
import System.Sensors.Internal.LmSensors
import System.Sensors.LmSensors.Types

newtype LmSensors = LmSensors ()

instance
  (MonadIO m) =>
  Sensors
    LmSensors
    ()
    SensorsChipName
    SensorsFeature
    SensorsSubfeature
    SensorsError
    m
  where
  sensorsInit :: (MonadIO m) => () -> m (Either SensorsError LmSensors)
  sensorsInit _ =
    liftIO $
      sensors_init nullPtr >>= checkRet >>= \case
        Left ret -> return $ Left ret
        Right _ -> return $ Right $ LmSensors ()
  sensorsDestroy :: (MonadIO m) => LmSensors -> m ()
  sensorsDestroy _ = liftIO $ sensors_cleanup >> return ()
  getChips ::
    (MonadIO m) =>
    LmSensors ->
    m (Either SensorsError [SensorsChipName])
  getChips _ =
    liftIO $
      (iterateAPI . sensors_get_detected_chips) nullPtr
        >>= mapM peek
        >>= return . Right
  getFeatures ::
    (MonadIO m) =>
    LmSensors ->
    SensorsChipName ->
    m (Either SensorsError [SensorsFeature])
  getFeatures _ chip = liftIO $
    with chip $ \chipPtr ->
      (iterateAPI . sensors_get_features) chipPtr
        >>= mapM peek
        >>= return . Right
  getSubFeatures ::
    (MonadIO m) =>
    LmSensors ->
    SensorsChipName ->
    SensorsFeature ->
    m (Either SensorsError [SensorsSubfeature])
  getSubFeatures _ chip feat = liftIO $
    with chip $ \chipPtr ->
      with feat $ \featPtr ->
        (iterateAPI $ sensors_get_all_subfeatures chipPtr featPtr)
          >>= mapM peek
          >>= return . Right
  getSubFeatureValue ::
    (MonadIO m) =>
    LmSensors ->
    SensorsChipName ->
    SensorsFeature ->
    SensorsSubfeature ->
    m (Either SensorsError SensorValue)
  getSubFeatureValue _ chip _ subfeat = liftIO $ alloca $ \ptr ->
    let nr = subfeatureNumber subfeat
     in with chip $ \chipPtr ->
          with 0 $ \valuePtr ->
            poke ptr nr
              >> sensors_get_value chipPtr nr valuePtr
              >>= checkRet
              >>= return
                ( Right
                    <$> DoubleValue
                    <$> peek (coerce valuePtr)
                )
