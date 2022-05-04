# Building daikin2mqtt

## For Production Use...
`gprbuild -Xmode=release`

## Prerequisites for builing from source
 * *daikin2mqtt* is written in Ada - you will need GNAT Ada version 9.4 or later installed to build the software.
* `ada-toml` from https://github.com/pmderodat/ada-toml
* MQTT-related...
  * `libmosquitto-dev`
  * `mosquitto` (mosquitto-ada) from https://github.com/persan/mosquitto-ada
* `libaws19-dev` (or later)
* `libgnatcoll-dev`

## Deployment
To deploy the binary Linux version you will need to ensure the following packages are installed...

* libmosquitto1
* libgnat-9
* libgnatcoll18
* libaws5
  
