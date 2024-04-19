module System.Sensors.Internal.LmSensors where

import Foreign
import Foreign.C (CString)
import Foreign.C.Types
import System.Sensors.LmSensors.Types (SensorsChipName, SensorsFeature, SensorsSubfeature)

foreign import ccall "sensors/sensors.h sensors_init"
  sensors_init :: Ptr CInt -> IO CInt

foreign import ccall "sensors/sensors.h sensors_cleanup"
  sensors_cleanup :: IO ()

foreign import ccall "sensors/sensors.h sensors_get_detected_chips"
  sensors_get_detected_chips :: CString -> Ptr CInt -> IO (Ptr SensorsChipName)

foreign import ccall "sensors/sensors.h sensors_get_features"
  sensors_get_features :: Ptr SensorsChipName -> Ptr CInt -> IO (Ptr SensorsFeature)

foreign import ccall "sensors/sensors.h sensors_get_all_subfeatures"
  sensors_get_all_subfeatures :: Ptr SensorsChipName -> Ptr SensorsFeature -> Ptr CInt -> IO (Ptr SensorsSubfeature)

foreign import ccall "sensors/sensors.h sensors_get_value"
  sensors_get_value :: Ptr SensorsChipName -> CInt -> Ptr CDouble -> IO CInt

foreign import ccall "sensors/sensors.h sensors_set_value"
  sensors_set_value :: Ptr SensorsChipName -> CInt -> CDouble -> IO CInt

iterateAPI :: (Ptr CInt -> IO (Ptr a)) -> IO [Ptr a]
iterateAPI f = go f 0
  where
    go :: (Ptr CInt -> IO (Ptr a)) -> CInt -> IO [Ptr a]
    go fun nr = do
      with nr $ \nrPtr -> do
        res <- fun nrPtr
        newNr <- peek nrPtr
        if res == nullPtr
          then return []
          else (res :) <$> go fun newNr