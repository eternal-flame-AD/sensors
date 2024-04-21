# Sensors

Haskell library for reading hardware sensors on your platform.

# Features

A cli interface for querying sensors on your platform.

# Usage

## Library

### Tutorial

TBD, for now see the haddock documentation and the implementation of the CLI (cmd/sensors.hs).

## sensors-hs CLI

### `dump` command

Enumerate all sensor backends and dump the sensor data. Useful for debugging.

#### Options

- `--backend` - Specify the sensor backends to use. Defaults to all available backends.

### `monitor` command

Regularly polls the sensors and prints the sensor data in JSON format.

#### Options

- `--interval` - Specify the interval in seconds to poll the sensors in tenths of a second. Defaults to 1 second (10).
- `--config` - Specify the configuration file to use. See [monitor.json](./monitor.json) for an example configuration.

## Xmobar Plugin

The library comes with an Xmobar plugin for displaying sensor data on your Xmobar status bar. Enable the `xmobar` flag when building the library to include the plugin.

Example Configuration:

```haskell
import qualified System.Sensors.Xmobar as S

myConfig =
    defaultConfig
      {
        ...
      , template = "%sensors%"
      , commands = [
            Run $
                S.XmobarSensors $
                S.Config
                    { S.configFragments =
                        [ S.FragmentText "Tctl: "
                        , S.FragmentSensor
                            (S.SensorSpec "lm-sensors" "k10temp" "temp1" "temp1_input")
                            ( Just $
                                S.ValueFormat
                                { S.colorHighColor = "red"
                                , S.colorLowColor = "green"
                                , S.colorHighValue = 85
                                , S.colorLowValue = 40
                                , S.formatStr = "%.1f"
                                }
                            )
                        , S.FragmentText "°C "
                        , S.FragmentText " GPU: "
                        , S.FragmentSensor
                            (S.SensorSpec "nvidia-smi" gpuId "gpu" "temperature.gpu")
                            ( Just $
                                S.ValueFormat
                                { S.colorHighColor = "red"
                                , S.colorLowColor = "green"
                                , S.colorHighValue = 85
                                , S.colorLowValue = 40
                                , S.formatStr = "%s °C"
                                }
                            )
                        , S.FragmentText " "
                        , S.FragmentSensor
                            (S.SensorSpec "nvidia-smi" gpuId "gpu" "utilization.gpu")
                            ( Just $
                                S.ValueFormat
                                { S.colorHighColor = "red"
                                , S.colorLowColor = "green"
                                , S.colorHighValue = 85
                                , S.colorLowValue = 40
                                , S.formatStr = "%s"
                                }
                            )
                        , S.FragmentText " "
                        , S.FragmentSensor
                            (S.SensorSpec "nvidia-smi" gpuId "gpu" "power.draw.average")
                            Nothing
                        ]
                    , S.configRate = 20
                    }
        ]
      }
```

# TODO

## Backend support

- [X] lm_sensors
- [X] nvidia-smi
- [ ] hwmon
- [ ] Windows API/WMI

