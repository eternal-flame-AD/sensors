{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{- |
Module: System.Sensors.LmSensors
Description: Implementation of the @Sensors@ typeclass for lm-sensors
License: Apache-2.0
Maintainer: yume@yumechi.jp
Stability: experimental
Portability: POSIX

@Sensors@ and @Monitorable@ instances for lm-sensors library.
-}
module System.Sensors.LmSensors (
  LmSensors,
  LmSensorsMonitor,
  LmSensorsMonitorException (..),
)
where

import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Coerce (coerce)
import Data.Maybe (fromJust, isJust)
import Foreign
import Foreign.C (peekCString)
import System.Sensors
import System.Sensors.Internal.LmSensors
import System.Sensors.LmSensors.Types
import System.Sensors.Monitor
import System.Sensors.Utils

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

data WatchTree = WatchTree [WatchChip]
  deriving (Show)

data WatchChip = WatchChip
  { watchChipNameString :: String
  , watchChipName :: SensorsChipName
  , watchChipFeatures :: [WatchFeature]
  }
  deriving (Show)

data WatchFeature = WatchFeature
  { watchFeatureNameString :: String
  , watchFeatureName :: SensorsFeature
  , watchFeatureSubfeatures :: [WatchSubfeature]
  }
  deriving (Show)

data WatchSubfeature = WatchSubfeature
  { watchSubfeatureNameString :: String
  , watchSubfeatureName :: SensorsSubfeature
  }
  deriving (Show)

-- | implementation of the @Monitorable@ typeclass for lm-sensors
data LmSensorsMonitor = LmSensorsMonitor
  { lmSensors :: LmSensors
  , cancel :: IO ()
  }

buildWatchTree :: LmSensors -> [MonitorSpec] -> IO WatchTree
buildWatchTree sensors specs = do
  chips <-
    getChips sensors >>= \case
      Left err -> error $ "Error getting chips: " ++ show err
      Right chips -> return chips
  watchChips <- buildWatchChips specs sensors chips
  return $ WatchTree watchChips
 where
  filterJust :: [Maybe a] -> [a]
  filterJust = map fromJust . filter isJust

  buildWatchChips :: [MonitorSpec] -> LmSensors -> [SensorsChipName] -> IO [WatchChip]
  buildWatchChips specs sensors chips = do
    mapM (buildWatchChip specs sensors) chips

  buildWatchChip ::
    [MonitorSpec] ->
    LmSensors ->
    SensorsChipName ->
    IO WatchChip
  buildWatchChip specs sensors chip = do
    chipNameString <- peekCString $ chipPrefix chip
    features <-
      getFeatures sensors chip >>= \case
        Left err -> error $ "Error getting features for chip " ++ show chip ++ ": " ++ show err
        Right features -> return features
    watchFeatures <-
      buildWatchTreeFeatures
        (filter (\spec -> monitorSpecChip spec == chipNameString) specs)
        sensors
        chip
        features
    return $ WatchChip chipNameString chip watchFeatures

  buildWatchTreeFeatures ::
    [MonitorSpec] ->
    LmSensors ->
    SensorsChipName ->
    [SensorsFeature] ->
    IO [WatchFeature]
  buildWatchTreeFeatures specs sensors chip features =
    filterJust <$> mapM (buildWatchTreeFeature specs sensors chip) features

  buildWatchTreeFeature ::
    [MonitorSpec] ->
    LmSensors ->
    SensorsChipName ->
    SensorsFeature ->
    IO (Maybe WatchFeature)
  buildWatchTreeFeature specs sensors chip feature = do
    featureNameString <- peekCString $ featureName feature

    if elem featureNameString $ map monitorSpecFeature specs
      then do
        subfeatures <-
          getSubFeatures sensors chip feature >>= \case
            Left err -> error $ "Error getting subfeatures: chip=" ++ show chip ++ ", feature=" ++ show feature ++ ": " ++ show err
            Right subfeatures -> return subfeatures
        watchSubfeatures <-
          buildWatchSubFeatures
            (filter (\spec -> monitorSpecFeature spec == featureNameString) specs)
            sensors
            chip
            feature
            subfeatures
        return $
          Just $
            WatchFeature featureNameString feature watchSubfeatures
      else return Nothing

  buildWatchSubFeatures ::
    [MonitorSpec] ->
    LmSensors ->
    SensorsChipName ->
    SensorsFeature ->
    [SensorsSubfeature] ->
    IO [WatchSubfeature]
  buildWatchSubFeatures specs sensors chip feature subfeatures = do
    filterJust <$> mapM (buildWatchSubFeature specs sensors chip feature) subfeatures

  buildWatchSubFeature ::
    [MonitorSpec] ->
    LmSensors ->
    SensorsChipName ->
    SensorsFeature ->
    SensorsSubfeature ->
    IO (Maybe WatchSubfeature)
  buildWatchSubFeature specs _ _ _ subfeature = do
    subfeatureNameString <-
      (peekCString $ subfeatureName subfeature)
    return $
      if elem subfeatureNameString $ map monitorSpecSubFeature specs
        then Just $ WatchSubfeature subfeatureNameString subfeature
        else Nothing

-- | Exception type for lm-sensors monitor
data LmSensorsMonitorException
  = UnknownChip String [String]
  | UnknownFeature String [String]
  | UnknownSubfeature String [String]
  | SensorError String
  deriving (Show)

instance Exception LmSensorsMonitorException

instance Monitorable LmSensorsMonitor where
  setupMonitor ::
    [MonitorSpec] ->
    Int ->
    ([(MonitorSpec, SensorValue)] -> IO ()) ->
    IO LmSensorsMonitor
  setupMonitor specs interval cb = do
    sensors <- sensorsInit @LmSensors ()
    case sensors of
      Left err -> error $ "Error initializing sensors: " ++ show err
      Right s -> do
        tree <- buildWatchTree s specs
        cancel <- forkForeverCancel $ do
          vals <- readMonitor specs tree s
          cb vals
          threadDelay (interval * 100000)
        return $ LmSensorsMonitor s cancel
   where
    readMonitor ::
      [MonitorSpec] -> WatchTree -> LmSensors -> IO [(MonitorSpec, SensorValue)]
    readMonitor specs tree sensors = do
      values <-
        mapM
          ( \spec ->
              accessTree
                tree
                (monitorSpecChip spec)
                (monitorSpecFeature spec)
                (monitorSpecSubFeature spec)
                >>= \value -> return (spec, value)
          )
          specs
      return values
     where
      accessTree :: WatchTree -> String -> String -> String -> IO SensorValue
      accessTree (WatchTree chips) chipName featureName subfeatureName = case (filter (\c -> watchChipNameString c == chipName) chips) of
        [] -> throwIO $ UnknownChip chipName $ map watchChipNameString chips
        chip : _ -> case (filter (\f -> watchFeatureNameString f == featureName) $ watchChipFeatures chip) of
          [] -> throwIO $ UnknownFeature featureName $ map watchFeatureNameString $ watchChipFeatures chip
          feature : _ -> case (filter (\s -> watchSubfeatureNameString s == subfeatureName) $ watchFeatureSubfeatures feature) of
            [] -> throwIO $ UnknownSubfeature subfeatureName $ map watchSubfeatureNameString $ watchFeatureSubfeatures feature
            subfeature : _ -> do
              val <- getSubFeatureValue sensors (watchChipName chip) (watchFeatureName feature) (watchSubfeatureName subfeature)
              case val of
                Left err -> throwIO $ SensorError $ show err
                Right v -> return v

  destroyMonitor :: LmSensorsMonitor -> IO ()
  destroyMonitor sensors = do
    cancel sensors
    sensorsDestroy $ lmSensors sensors
