{- |
Module: System.Sensors.LmSensors.Types
Description: Type definitions for lm-sensors bindings
License: Apache-2.0
Maintainer: yume@yumechi.jp
Stability: experimental
Portability: POSIX
-}
module System.Sensors.LmSensors.Types where

import Control.Monad.IO.Class (MonadIO)
import Foreign
import Foreign.C (CString)
import Foreign.C.Types

{- | Error codes returned by the lm_sensors library
     The corresponding error codes and messages are defined in <sensors/error.h>
-}
data SensorsError = ErrWildcards | ErrNoEntry | ErrAccessR | ErrKernel | ErrDivZero | ErrChipName | ErrBusName | ErrParse | ErrAccessW | ErrIO | ErrRecursion
  deriving (Eq, Show)

errorMessage :: SensorsError -> String
errorMessage ErrWildcards = "Wildcard found in chip name"
errorMessage ErrNoEntry = "No such subfeature known"
errorMessage ErrAccessR = "Can't read"
errorMessage ErrKernel = "Kernel interface error"
errorMessage ErrDivZero = "Divide by zero"
errorMessage ErrChipName = "Can't parse chip name"
errorMessage ErrBusName = "Can't parse bus name"
errorMessage ErrParse = "General parse error"
errorMessage ErrAccessW = "Can't write"
errorMessage ErrIO = "I/O error"
errorMessage ErrRecursion = "Evaluation recurses too deep"

instance Enum SensorsError where
  fromEnum ErrWildcards = 1
  fromEnum ErrNoEntry = 2
  fromEnum ErrAccessR = 3
  fromEnum ErrKernel = 4
  fromEnum ErrDivZero = 5
  fromEnum ErrChipName = 6
  fromEnum ErrBusName = 7
  fromEnum ErrParse = 8
  fromEnum ErrAccessW = 9
  fromEnum ErrIO = 10
  fromEnum ErrRecursion = 11
  toEnum 1 = ErrWildcards
  toEnum 2 = ErrNoEntry
  toEnum 3 = ErrAccessR
  toEnum 4 = ErrKernel
  toEnum 5 = ErrDivZero
  toEnum 6 = ErrChipName
  toEnum 7 = ErrBusName
  toEnum 8 = ErrParse
  toEnum 9 = ErrAccessW
  toEnum 10 = ErrIO
  toEnum 11 = ErrRecursion
  toEnum _ = error "Invalid SensorsError"
  enumFrom begin = [begin .. maxBound]
  enumFromThen begin next = [begin, next .. maxBound]

instance Bounded SensorsError where
  minBound = ErrWildcards
  maxBound = ErrRecursion

checkRet :: (MonadIO m) => CInt -> m (Either SensorsError ())
checkRet ret
  | ret == 0 = return $ Right ()
  | otherwise = return $ Left $ toEnum $ fromIntegral ret

data SensorsBusId = SensorsBusId
  { busType :: CShort
  , busNr :: CShort
  }
  deriving (Show)

instance Storable SensorsBusId where
  sizeOf _ = 4
  alignment _ = 2
  peek ptr = do
    busTypeV <- peekByteOff ptr 0
    busNrV <- peekByteOff ptr 2
    return $ SensorsBusId busTypeV busNrV
  poke ptr (SensorsBusId busTypeV busNrV) = do
    pokeByteOff ptr 0 busTypeV
    pokeByteOff ptr 2 busNrV

data SensorsChipName = SensorsChipName
  { chipPrefix :: CString
  , chipBus :: SensorsBusId
  , chipAddr :: CInt
  , chipPath :: CString
  }
  deriving (Show)

instance Storable SensorsChipName where
  sizeOf _ = 24
  alignment _ = 8
  peek ptr = do
    prefix <- peekByteOff ptr 0
    busid <- peekByteOff ptr 8
    addr <- peekByteOff ptr 12
    path <- peekByteOff ptr 16
    return $ SensorsChipName prefix busid addr path
  poke ptr (SensorsChipName prefix busid addr path) = do
    pokeByteOff ptr 0 prefix
    pokeByteOff ptr 8 busid
    pokeByteOff ptr 12 addr
    pokeByteOff ptr 16 path

type FeatureType = CInt

data SubFeatureFlag = SensorsModeR | SensorsModeW | SensorsComputeMapping
  deriving (Show, Eq)

newtype SubFeatureFlags = SubFeatureFlags [SubFeatureFlag]
  deriving (Show, Eq)

hasFlag :: SubFeatureFlag -> SubFeatureFlags -> Bool
hasFlag flag (SubFeatureFlags flags) = flag `elem` flags

encodeSubFeatureFlags :: SubFeatureFlags -> CInt
encodeSubFeatureFlags (SubFeatureFlags flags) =
  foldl
    ( \acc flag ->
        if flag == SensorsModeR
          then acc .|. 1
          else
            if flag == SensorsModeW
              then acc .|. 2
              else
                if flag == SensorsComputeMapping
                  then acc .|. 4
                  else acc
    )
    0
    flags

decodeSubFeatureFlags :: CInt -> SubFeatureFlags
decodeSubFeatureFlags flags =
  SubFeatureFlags
    [ snd flag
    | flag <- [(1, SensorsModeR), (2, SensorsModeW), (4, SensorsComputeMapping)]
    , flags .&. (fst flag) /= 0
    ]
instance Storable SubFeatureFlags where
  sizeOf _ = 4
  alignment _ = 4
  peek ptr = do
    flags <- peekByteOff ptr 0
    return $ decodeSubFeatureFlags flags
  poke ptr (SubFeatureFlags flags) = do
    pokeByteOff ptr 0 $ encodeSubFeatureFlags $ SubFeatureFlags flags

data SensorsFeature = SensorsFeature
  { featureName :: CString
  , featureNumber :: CInt
  , sensorFeatureType :: FeatureType
  , firstSubfeature :: CInt
  , sensorsFeaturePadding1 :: CInt
  }
  deriving (Show)

instance Storable SensorsFeature where
  sizeOf _ = 24
  alignment _ = 8
  peek ptr = do
    name <- peekByteOff ptr 0
    number <- peekByteOff ptr 8
    ftype <- peekByteOff ptr 12
    first <- peekByteOff ptr 16
    padding <- peekByteOff ptr 20
    return $ SensorsFeature name number ftype first padding
  poke ptr (SensorsFeature name number ftype first padding) = do
    pokeByteOff ptr 0 name
    pokeByteOff ptr 8 number
    pokeByteOff ptr 12 ftype
    pokeByteOff ptr 16 first
    pokeByteOff ptr 20 padding

data SensorsSubfeature = SensorsSubfeature
  { subfeatureName :: CString
  , subfeatureNumber :: CInt
  , sensorSubfeatureType :: FeatureType
  , sensorSubfeatureMapping :: CInt
  , sensorSubfeatureFlags :: CInt
  }
  deriving (Show)

instance Storable SensorsSubfeature where
  sizeOf _ = 24
  alignment _ = 8
  peek ptr = do
    name <- peekByteOff ptr 0
    number <- peekByteOff ptr 8
    ftype <- peekByteOff ptr 12
    mapping <- peekByteOff ptr 16
    flags <- peekByteOff ptr 20
    return $ SensorsSubfeature name number ftype mapping flags
  poke ptr (SensorsSubfeature name number ftype mapping flags) = do
    pokeByteOff ptr 0 name
    pokeByteOff ptr 8 number
    pokeByteOff ptr 12 ftype
    pokeByteOff ptr 16 mapping
    pokeByteOff ptr 20 flags