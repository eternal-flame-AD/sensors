{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- |
Module: System.Sensors.NvidiaSMI
Description: Implementation of the @Sensors@ typeclass for nvidia-smi
License: Apache-2.0
Maintainer: yume@yumechi.jp
Stability: experimental
Portability: portable

@Sensors@ and @Monitorable@ instances for nvidia-smi CLI tool.
-}
module System.Sensors.NvidiaSMI (
    NvidiaSMI,
    NvidiaSMIMonitor,
    NvidiaOptions (..),
    NvidiaChip (..),
    NvidiaFeature (..),
    NvidiaSubfeature (..),
    getAllGpuQueries,
    getAllGpus,
    waitForReady,
)
where

import Control.Concurrent (readChan, readMVar, tryReadMVar)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Default
import Data.List (elemIndex, nub)
import Data.Maybe (listToMaybe)
import GHC.IO.Handle
import System.Process (waitForProcess)
import System.Sensors
import System.Sensors.Internal.NvidiaSMI.Parser
import System.Sensors.Internal.NvidiaSMI.Process
import System.Sensors.Monitor
import System.Sensors.NvidiaSMI.Types
import System.Sensors.Utils
import Text.ParserCombinators.ReadP

baseSubFeatureName :: String -> String
baseSubFeatureName =
    \s -> case parser s of
        [(x, "")] -> x
        _ -> s
  where
    parser =
        readP_to_S $
            munch1 (`elem` ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ ['-', '_', '.'])
                <* munch (const True)
                <* eof

-- | Get all available GPU queries
getAllGpuQueries :: IO [String]
getAllGpuQueries = do
    (stdout, handle) <- spawnQueryGpuHelpProcess
    parseResult <- parseHelpQueryGpu stdout
    hClose stdout
    _ <- waitForProcess handle
    return parseResult

-- | Get all GPU UUIDs
getAllGpus :: IO [String]
getAllGpus = do
    (stdout, handle) <- spawnListGpusProcess
    parseResult <- parseGPUUuids stdout
    hClose stdout
    _ <- waitForProcess handle
    return parseResult

{- | A handle to the nvidia-smi process.
 pass this to the sensors functions.
-}
data NvidiaSMI = NvidiaSMI
    { nvidiaSmiProcess :: NvidiaSmiProcess
    , nvidiaStartArgs :: ProcessStartArgs
    }

waitSMI :: NvidiaSMI -> IO ()
waitSMI smi = readChan $ processUpdateChan $ nvidiaSmiProcess smi

{- | Options for the nvidia-smi backend.
   If GPU UUID is not specified, the first GPU is used.
   Default loop interval is 1000ms. If you want to disable the loop, set it to Nothing for a one-shot query.
   If queryGpu is not specified, all available queries are used. Default is Nothing.
   If captureStderr is True, stderr is captured. Default is False.
   If noUnits is True, units are not included in the output. Default is False.
-}
data NvidiaOptions = NvidiaOptions
    { nvidiaOptionsGpuId :: Maybe String
    , nvidiaOptionsLoopMs :: Maybe Int
    , nvidiaOptionsQueryGpuItems :: Maybe [String]
    , nvidiaOptionsCaptureStderr :: Bool
    , nvidiaOptionsNoUnits :: Bool
    }

instance Default NvidiaOptions where
    def = NvidiaOptions Nothing (Just 1000) Nothing False False

newtype NvidiaChip = NvidiaChip String deriving (Show, Eq)

newtype NvidiaFeature = NvidiaFeature String deriving (Show, Eq)

newtype NvidiaSubfeature = NvidiaSubfeature String deriving (Show, Eq)

instance
    (MonadIO m) =>
    Sensors
        NvidiaSMI
        NvidiaOptions
        NvidiaChip
        NvidiaFeature
        NvidiaSubfeature
        NvidiaError
        m
    where
    sensorsInit :: (MonadIO m) => NvidiaOptions -> m (Either NvidiaError NvidiaSMI)
    sensorsInit options = do
        gpuUuid <-
            case nvidiaOptionsGpuId options of
                Nothing -> do
                    gpus <- liftIO getAllGpus
                    case gpus of
                        [] -> pure $ Left NoGPUFound
                        x : _ -> pure $ Right x
                Just opt -> pure $ Right $ opt
        queryItems <-
            case nvidiaOptionsQueryGpuItems options of
                Nothing -> liftIO getAllGpuQueries
                Just opt -> pure $ opt

        case gpuUuid of
            Left err -> return $ Left err
            Right gpuUuid' -> do
                let startArgs =
                        ProcessStartArgs
                            { processStartGpuId = gpuUuid'
                            , processStartLoopMs = nvidiaOptionsLoopMs options
                            , queryGpu = queryItems
                            , processFormat =
                                if nvidiaOptionsNoUnits options then "csv,nounits" else "csv"
                            , captureStderr = nvidiaOptionsCaptureStderr options
                            }
                process <- liftIO $ spawnProcess startArgs
                return $ Right $ NvidiaSMI process startArgs

    sensorsDestroy :: (MonadIO m) => NvidiaSMI -> m ()
    sensorsDestroy smi = liftIO $ (killProcess . nvidiaSmiProcess) smi

    getChips :: (MonadIO m) => NvidiaSMI -> m (Either NvidiaError [NvidiaChip])
    getChips smi = return $ Right [NvidiaChip $ processStartGpuId $ nvidiaStartArgs smi]

    getFeatures ::
        (MonadIO m) => NvidiaSMI -> NvidiaChip -> m (Either NvidiaError [NvidiaFeature])
    getFeatures _ _ = do return $ Right [NvidiaFeature "gpu"]

    getSubFeatures ::
        (MonadIO m) =>
        NvidiaSMI ->
        NvidiaChip ->
        NvidiaFeature ->
        m (Either NvidiaError [NvidiaSubfeature])
    getSubFeatures smi c feat = do
        let (NvidiaChip chip) = c
        let (NvidiaFeature featS) = feat
        if featS /= "gpu"
            then return $ Left NotFound
            else do
                if chip /= processStartGpuId (nvidiaStartArgs smi)
                    then return $ Left WrongChipName
                    else
                        return $
                            Right
                                ( map (NvidiaSubfeature . baseSubFeatureName) $
                                    processFormatHeader $
                                        nvidiaSmiProcess smi
                                )

    getSubFeatureValue ::
        (MonadIO m) =>
        NvidiaSMI ->
        NvidiaChip ->
        NvidiaFeature ->
        NvidiaSubfeature ->
        m (Either NvidiaError SensorValue)
    getSubFeatureValue smi c feat subfeat = do
        let (NvidiaChip chip) = c
        let (NvidiaFeature featS) = feat
        if featS /= "gpu"
            then return $ Left NotFound
            else do
                let (NvidiaSubfeature subfeatS) = subfeat
                let subfeatS' = baseSubFeatureName subfeatS
                if chip /= processStartGpuId (nvidiaStartArgs smi)
                    then return $ Left WrongChipName
                    else do
                        res <- liftIO $ tryReadMVar $ processLastOutput $ nvidiaSmiProcess smi
                        case res of
                            Nothing -> return $ Left NotReady
                            Just (NvidiaSmiQueryResult _ res') -> do
                                case elemIndex subfeatS' $ map baseSubFeatureName $ processFormatHeader $ nvidiaSmiProcess smi of
                                    Nothing -> return $ Left NotFound
                                    Just idx -> do
                                        let value = res' !! idx
                                        return $ Right $ StringValue value

waitForReady :: NvidiaSMI -> IO ()
waitForReady smi = (readMVar $ processLastOutput $ nvidiaSmiProcess smi) >> return ()

-- | implementation of the @Monitorable@ typeclass for nvidia-smi
data NvidiaSMIMonitor = NvidiaSMIMonitor
    { nvidiaSMIMonitorProcess :: NvidiaSMI
    , nvidiaSMIMonitorCancel :: IO ()
    }

instance Monitorable NvidiaSMIMonitor where
    setupMonitor ::
        [MonitorSpec] ->
        Int ->
        ([(MonitorSpec, SensorValue)] -> IO ()) ->
        IO NvidiaSMIMonitor
    setupMonitor spec interval cb = do
        let chips = map monitorSpecChip spec
        if length (nub chips) > 1
            then error "Multiple GPUs are not supported in one monitor"
            else return ()
        let chip = listToMaybe chips
        let features = map monitorSpecSubFeature spec
        smi <-
            sensorsInit
                def
                    { nvidiaOptionsGpuId = chip
                    , nvidiaOptionsQueryGpuItems = Just features
                    , nvidiaOptionsLoopMs = Just $ interval * 100
                    }
        case smi of
            Left err -> error $ show err
            Right smi' -> do
                waitForReady smi'
                cancel <- forkForeverCancel $ do
                    waitSMI smi'
                    vals <- readMonitor smi' spec
                    cb vals
                return $ NvidiaSMIMonitor smi' cancel
      where
        readMonitor :: NvidiaSMI -> [MonitorSpec] -> IO [(MonitorSpec, SensorValue)]
        readMonitor smi specs = do
            vals <-
                mapM
                    ( \spec' -> do
                        let chip = monitorSpecChip spec'
                        let feat = monitorSpecFeature spec'
                        let subfeat = monitorSpecSubFeature spec'
                        res <- getSubFeatureValue smi (NvidiaChip chip) (NvidiaFeature feat) (NvidiaSubfeature subfeat)
                        case res of
                            Left err -> error $ show err
                            Right val -> return (spec', val)
                    )
                    specs
            return vals

    destroyMonitor :: NvidiaSMIMonitor -> IO ()
    destroyMonitor mon = do
        nvidiaSMIMonitorCancel mon
        sensorsDestroy $ nvidiaSMIMonitorProcess mon
