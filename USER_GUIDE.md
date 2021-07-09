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

(From here on we assume you have set the base_topic to "daikin2mqtt".)

There are three main types of message topic:

1. Device status updates and responses to requests are published to `daikin2mqtt/FRIENDLY_NAME/<type>`
2. Commands you send to inverters via `daikin2mqtt/FRIENDLY_NAME/set`
3. Requests for inverter state via `daikin2mqtt/FRIENDLY_NAME/get`

On start-up, the simple string "Started" is sent to the `daikin2mqtt/status` topic.

### Set Subtopics
The only subtopic currently defined for `set` is `control`, you must supply a control message.

Eg. `daikin2mqtt/Spare_Bedroom/set/control`

### Get Subtopics
The subtopics for `get` are `basic`, `control` and `sensor`.

Eg. `daikin2mqtt/Steves_Room/get/basic`

## MQTT Messages

Messages (payloads) are all in JSON format.

### Basic
```
{
    "detected":         true|false,
    "online":           true|false,
    "firmware_version": "<string>",
    "power":            true|false,
    "error":            <integer>,
    "adaptor_led":      true|false,
    "holiday_mode":     true|false,
    "group_mode":       true|false,
    "group_name":       "<string>"
}
```

### Control Report
Published to subtopic `/controls`
```
{
    "power":            true|false,
    "mode":             "<mode-string>",
    "set_temp":         <integer>,
    "set_humidity":     <integer>,
    "fan_rate":         "<fan-rate-string>",
    "fan_sweep":        "<fan-sweep-string>"
}   
```
Where:

`<mode-string>` is one of "AUTO", "AUTO 1", "DEHUMIDIFY", "COOL", "HEAT", "MODE 5", "FAN", "AUTO 2"

`<fan-rate-string>` is one of "AUTO", "SILENT", "LEVEL_1", ... "LEVEL_5"

`<fan-sweep-string>` is one of "OFF", "VERTICAL", "HORIZONTAL", "BOTH"

In dehumidify mode, some units return a set temperature of "M", Daikink2MQTT will return 0 in this case.

In some modes, set temperature and set humidity are reported as "--", Daikink2MQTT will return 0 in these cases.

### Sensor Message
Published to subtopic `/sensors`
```
{
    "unit_temp":        <float, 1 d.p.>,
    "unit_humidity":    <integer>,
    "ext_temp":         <float, 1 d.p.>,
    "error":            <integer>
}
```
Not all units return external temperatures (some only when powered on), Daikin2MQTT will return a dummy
value of -99.0 when no temperature is reported.

Likewise, not all units report humidity, Daikin2MQTT will return a value of 0 if none is reported.

