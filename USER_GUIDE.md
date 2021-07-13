# Daikin2MQTT User Guide

## Configuration

Configuration is performed via a single TOML file, when you start the program you must pass it the name of your configuration file eg. 

`daikin2mqtt -conf /home/ha_user/daikin2mqtt.toml`

Here is a sample configuration showing all available options...
```
# All configuration options

[mqtt]
base_topic = "daikin2mqtt"          # Required: MQTT base topic
broker = "localhost"                # Required: MQTT broker
port = 1883                         # Required: MQTT port
# user = "my_user"                  # Optional: MQTT broker user (default: nothing) - NYI !
# password = "my_password"          # Optional: MQTT broker password (default: nothing) - NYI !

[daikin]
discovery_timeout = 5               # Required: No. secs to wait for units to respond
update_period = 60                  # Required: Pause (s) between fetching updates from inverters

[[inverter]]
  mac = "C0E434E69F27"              # Required: The unpunctuated MAC address of the unit
  friendly_name = "Steve_Office"    # Required: Will form part of the MQTT topic
  
[[inverter]]
  mac = "C0E434E60BC6"             
  friendly_name = "Spare_Bedroom"  
```

## MQTT Topics

N.B. Henceforth we assume you have set the base_topic to "daikin2mqtt".

There are three main types of message topic:

1. Device status updates and responses to requests are published to `daikin2mqtt/FRIENDLY_NAME/<type>`
2. Requests for inverter state via `daikin2mqtt/FRIENDLY_NAME/get`
3. Commands you send to inverters via `daikin2mqtt/FRIENDLY_NAME/set`
   
On start-up, the simple string "Started" is sent to the `daikin2mqtt/status` topic.

Currently, *daikin2mqtt* does not store live data internally - each time a `get` request is 
received the appropriate unit is queried for the data.

### Get Subtopics
The subtopics for `get` are `basic`, `controls`, [`online` ??? - tbd], and  `sensors` 

Eg. `daikin2mqtt/Steves_Room/get/sensors`

### Set Subtopics
The only subtopic currently defined for `set` is `controls`, you must supply a control message.

Eg. `daikin2mqtt/Spare_Bedroom/set/controls`

The JSON-formatted control message may contain any number of the fields shown below
* setting the timestamp is meaningless and will be ignored
* you can set just one, several, or all of the controls
* unset controls will be left as-is

## MQTT Messages

Messages (payloads) are all in JSON format.

### Basic
```
{
    "firmware_version": "<string>",
    "power":            true|false,
    "error":            <integer>,
    "adaptor_led":      true|false,
    "holiday_mode":     true|false,
    "group_mode":       true|false,
    "group_name":       "<string>",
    "timestamp":        "<HH:MM:SS>"
}
```
The timestamp records when the data were collected from the unit.

### Controls
Published to subtopic `/controls`
```
{
    "power":            true|false,
    "mode":             "<mode-string>",
    "set_temp":         <integer>,
    "set_humidity":     <integer>,
    "fan_rate":         "<fan-rate-string>",
    "fan_sweep":        "<fan-sweep-string>",
    "timestamp":        "<HH:MM:SS>"
}   
```
Where:

`<mode-string>` is one of "AUTO", "AUTO 1", "DEHUMIDIFY", "COOL", "HEAT", "MODE 5", "FAN", "AUTO 2"

`<fan-rate-string>` is one of "AUTO", "SILENT", "LEVEL_1", ... "LEVEL_5"

`<fan-sweep-string>` is one of "OFF", "VERTICAL", "HORIZONTAL", "BOTH"

In dehumidify mode, some units return a set temperature of "M", *daikin2mqtt* will return 0 in this case.

In some modes, set temperature and set humidity are reported as "--", *daikin2mqtt* will return 0 in these cases.

The timestamp records when the data were collected from the unit. Do not include the timestamp field in a `set/controls` request.

### Sensors
Published to subtopic `/sensors`
```
{
    "unit_temp":        <float, 1 d.p.>,
    "unit_humidity":    <integer>,
    "ext_temp":         <float, 1 d.p.>,
    "error":            <integer>,
    "timestamp":        "<HH:MM:SS>"
}
```
Not all units return external temperatures (some only when powered on), *daikin2mqtt* will return a dummy
value of -99.0 when no temperature is reported.

Likewise, not all units report humidity, *daikin2mqtt* will return a value of 0 if none is reported.

The timestamp records when the data were collected from the unit.