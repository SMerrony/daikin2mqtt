// This file is part of Daikin2MQTT.
// Copyright © 2021 Stephen Merrony

// Daikin2MQTT is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// Daikin2MQTT is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with Daikin2MQTT.  If not, see <https://www.gnu.org/licenses/>.

package daikin

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"log"
	"net"
	"net/http"
	"net/url"
	"strconv"
	"strings"
	"time"

	"github.com/SMerrony/daikin2mqtt/mqtt"
)

type DaikinConfT struct {
	Discovery_Timeout int
	Update_Period     int
}

type InverterConfT struct {
	Mac           string
	Friendly_Name string
}

// these are the known Daikin 'end-points'
const (
	getBasicInfo    = "/common/basic_info"
	getRemoteMethod = "/common/get_remote_method"
	getModelInfo    = "/aircon/get_model_info"
	getControlInfo  = "/aircon/get_control_info"
	getSchedTimer   = "/aircon/get_scdtimer"
	getSensorInfo   = "/aircon/get_sensor_info"
	getWeekPower    = "/aircon/get_week_power"
	getYearPower    = "/aircon/get_year_power"
	getWeekPowerExt = "/aircon/get_week_power_ex"
	getYearPowerExt = "/aircon/get_year_power_ex"
	setControlInfo  = "/aircon/set_control_info"
)

const setControlFmt = "?pow=%s&mode=%s&stemp=%s&f_rate=%s&f_dir=%s&shum=%s"

var modeArr = [...]string{"AUTO", "AUTO 1", "DEHUMIDIFY", "COOL", "HEAT", "MODE 5", "FAN", "AUTO 2"}
var fanRateMap = map[string]string{
	"A": "AUTO",
	"B": "SILENT",
	"3": "LEVEL_1",
	"4": "LEVEL_2",
	"5": "LEVEL_3",
	"6": "LEVEL_4",
	"7": "LEVEL_5"}
var inverseFanRateMap = map[string]string{
	"AUTO":    "A",
	"SILENT":  "B",
	"LEVEL_1": "3",
	"LEVEL_2": "4",
	"LEVEL_3": "5",
	"LEVEL_4": "6",
	"LEVEL_5": "7"}
var fanDirArr = [...]string{"OFF", "VERTICAL", "HORIZONTAL", "BOTH"}
var inverseFanDirMap = map[string]int{
	"OFF":        0,
	"VERTICAL":   1,
	"HORIZONTAL": 2,
	"BOTH":       3}

const (
	udpPort  = ":30050"
	udpQuery = "DAIKIN_UDP" + getBasicInfo
)

type BasicInfoT struct {
	address          string
	mac_address      string
	firmware_version string
	adaptor_version  string // hex, decode unknown
	powered_on       bool
	error_code       int
	name             string
	adaptor_type     int
	adaptor_led      bool
	holiday_mode     bool
	group_mode       bool
	group_name       string
	timestamp        time.Time
}

type ControlInfoT struct {
	retOK       bool
	power       bool      // `json:"power"`
	mode        string    // `json:"mode"`
	setTemp     int       // `json:"set_temp"`
	setHumidity int       // `json:"set_humidity"`
	fanRate     string    // `json:"fan_rate"`
	fanSweep    string    // `json:"fan_sweep"`
	timestamp   time.Time // `json:"timestamp"`
}

type SensorInfoT struct {
	retOK        bool
	unitTemp     float32
	unitHumidity float32
	extTemp      float32
	errorCode    int
	timestamp    time.Time
}

type InverterT struct {
	friendlyName string
	configured,
	detected,
	online bool
	basicInfo BasicInfoT // the Daikin 'basic' info returned when we detect the unit
	// SensorInfo  SensorInfoT
	// controlInfo ControlInfoT
}

type DaikinT struct {
	conf            DaikinConfT
	inverters       map[string]InverterT // mapped by MAC
	invertersByName map[string]string    // map friendly name to MAC
	httpReqClient   *http.Client
	mq              mqtt.MQTT_T
}

// Create sets up a new DaikinT struct and adds any configured inverters
func Create(dConf DaikinConfT, iConf []InverterConfT, mq mqtt.MQTT_T) (d DaikinT) {
	d.conf = dConf
	d.inverters = make(map[string]InverterT)
	d.invertersByName = make(map[string]string)
	for _, c := range iConf {
		if _, found := d.inverters[c.Mac]; found {
			log.Fatalln("ERROR: Cannot configure the same inverter more than once")
		}
		var i InverterT
		i.friendlyName = c.Friendly_Name
		i.configured = true
		i.detected = false
		i.online = false
		d.inverters[c.Mac] = i
		d.invertersByName[c.Friendly_Name] = c.Mac
	}
	log.Printf("INFO: %d Inverters found in configuration", len(d.inverters))
	d.httpReqClient = &http.Client{
		Timeout: time.Second, // TODO parm?
	}
	d.mq = mq
	return d
}

func (d *DaikinT) Start(conf DaikinConfT) {
	//d.conf = conf
}

func parseBasicInfo(buffer []byte) (BI BasicInfoT) {
	var key, val string
	var i64 int64
	inKey := true

	for i, c := range buffer {
		if c == '=' { // switch from key to value collection
			inKey = false
		} else if (c == ',') || (i == len(buffer)-1) { // we should have a complete key and value
			switch key {
			case "ver":
				BI.firmware_version = val
			case "rev":
				BI.adaptor_version = val
			case "pow":
				BI.powered_on = (val == "1")
			case "err":
				i64, _ = strconv.ParseInt(val, 10, 32)
				BI.error_code = int(i64)
			case "name":
				BI.name, _ = url.QueryUnescape(val)
			case "mac":
				BI.mac_address = val
			case "adp_kind":
				i64, _ = strconv.ParseInt(val, 10, 16)
				BI.adaptor_type = int(i64)
			case "led":
				BI.adaptor_led = (val == "1")
			case "en_hol":
				BI.holiday_mode = (val == "1")
			case "en_grp":
				BI.group_mode = (val == "1")
			case "grp_name":
				BI.group_name = val
			}
			// reset for the next key/val pair
			inKey = true
			key = ""
			val = ""
		} else { // carry on building up the key or val
			if inKey {
				key += string([]byte{c})
			} else {
				val += string([]byte{c})
			}
		}
	} // end for
	BI.timestamp = time.Now()
	return BI
}

// boolToJSON converts a bool into JSON
func boolToJSON(val bool) string {
	if val {
		return "true"
	} else {
		return "false"
	}
}

func basicInfoToJSON(BI BasicInfoT) (JSON string) {
	JSON = "{\"firmware_version\":\"" + BI.firmware_version + "\"" +
		",\"power\":" + boolToJSON(BI.powered_on) +
		",\"error\":" + strconv.Itoa(BI.error_code) +
		",\"adaptor_led\":" + boolToJSON(BI.adaptor_led) +
		",\"holiday_mode\":" + boolToJSON(BI.holiday_mode) +
		",\"group_mode\":" + boolToJSON(BI.group_mode) +
		",\"group_name\":\"" + BI.group_name + "\"" +
		",\"timestamp\":\"" + BI.timestamp.Format("15:04:05") + "\"}"
	return JSON
}

func parseControlInfo(buffer []byte) (CI ControlInfoT) {
	var key, val string
	var i64 int64
	var f64 float64
	inKey := true
	for i, c := range buffer {
		if c == '=' { // switch from key to value collection
			inKey = false
		} else if (c == ',') || (i == len(buffer)-1) { // we should have a complete key and value
			switch key {
			case "ret":
				CI.retOK = (val == "OK") // uppercase response from Daikin
			case "pow":
				CI.power = (val == "1")
			case "mode":
				i64, _ = strconv.ParseInt(val, 10, 8)
				CI.mode = modeArr[int(i64)]
			case "stemp":
				// stemp is returned as a float, but is always integral, 2 exceptions...
				if (val == "M") || (val == "--") {
					CI.setTemp = 0
				} else {
					f64, _ = strconv.ParseFloat(val, 32)
					CI.setTemp = int(f64)
				}
			case "shum":
				// as per stemp
				if val == "--" {
					CI.setHumidity = 0
				} else {
					f64, _ = strconv.ParseFloat(val, 32)
					CI.setHumidity = int(f64)
				}
			case "f_rate":
				CI.fanRate = fanRateMap[val]
			case "f_dir":
				i64, _ = strconv.ParseInt(val, 10, 8)
				CI.fanSweep = fanDirArr[int(i64)]
			}
			// reset for the next key/val pair
			inKey = true
			key = ""
			val = ""
		} else { // carry on building up the key or val
			if inKey {
				key += string([]byte{c})
			} else {
				val += string([]byte{c})
			}
		}
	} // end for
	CI.timestamp = time.Now()
	return CI
}

func controlInfoToJSON(CI ControlInfoT) (JSON string) {
	JSON = "{\"power\":"
	if CI.power {
		JSON += "true"
	} else {
		JSON += "false"
	}
	JSON += ",\"mode\":\"" + CI.mode + "\"" +
		",\"set_temp\":" + strconv.Itoa(CI.setTemp) +
		",\"set_humidity\":" + strconv.Itoa(CI.setHumidity) +
		",\"fan_rate\":\"" + CI.fanRate + "\"" +
		",\"fan_sweep\":\"" + CI.fanSweep + "\"" +
		",\"timestamp\":\"" + CI.timestamp.Format("15:04:05") + "\"}"
	return JSON
}

func parseSensorInfo(buffer []byte) (SI SensorInfoT) {
	var key, val string
	var i64 int64
	var f64 float64
	inKey := true
	for i, c := range buffer {
		if c == '=' { // switch from key to value collection
			inKey = false
		} else if (c == ',') || (i == len(buffer)-1) { // we should have a complete key and value
			switch key {
			case "ret":
				SI.retOK = (val == "OK") // uppercase response from Daikin
			case "htemp":
				f64, _ = strconv.ParseFloat(val, 32)
				SI.unitTemp = float32(f64)
			case "hhum":
				if val == "-" {
					SI.unitHumidity = 0.0
				} else {
					f64, _ = strconv.ParseFloat(val, 32)
					SI.unitHumidity = float32(f64)
				}
			case "otemp":
				if val == "-" {
					SI.extTemp = -99.0
				} else {
					f64, _ = strconv.ParseFloat(val, 32)
					SI.extTemp = float32(f64)
				}
			case "err":
				i64, _ = strconv.ParseInt(val, 10, 32)
				SI.errorCode = int(i64)
			}
			// reset for the next key/val pair
			inKey = true
			key = ""
			val = ""
		} else { // carry on building up the key or val
			if inKey {
				key += string([]byte{c})
			} else {
				val += string([]byte{c})
			}
		}
	} // end for
	SI.timestamp = time.Now()
	return SI
}

func sensorInfoToJSON(SI SensorInfoT) (JSON string) {
	JSON = fmt.Sprintf("{\"unit_temp\":%3.1f,\"unit_humidity\":%d,\"ext_temp\":%3.1f,\"error\":%d,\"timestamp\":\"%s\"}",
		SI.unitTemp, int(SI.unitHumidity), SI.extTemp, SI.errorCode, SI.timestamp.Format("15:04:05"))
	return JSON
}

func (d *DaikinT) Discover() {
	pc, err := net.ListenPacket("udp4", udpPort)
	if err != nil {
		panic(err)
	}
	defer pc.Close()
	addr, err := net.ResolveUDPAddr("udp4", "255.255.255.255"+udpPort)
	if err != nil {
		panic(err)
	}
	_, err = pc.WriteTo([]byte(udpQuery), addr)
	if err != nil {
		panic(err)
	}
	pc.SetReadDeadline(time.Now().Add(time.Duration(d.conf.Discovery_Timeout) * time.Second))
	// this loop will timeout rather than complete...
	for {
		buf := make([]byte, 1024)
		n, unitAddr, err := pc.ReadFrom(buf)
		if err != nil {
			// normal timeout
			if strings.HasSuffix(err.Error(), "timeout") {
				return
			}
			panic(err)
		}
		if !strings.HasSuffix(unitAddr.String(), udpPort) {
			var (
				//i  InverterT
				bi BasicInfoT
			)
			host, _, _ := net.SplitHostPort(unitAddr.String())
			bi = parseBasicInfo(buf[:n])
			bi.address = "http://" + host + ":80"

			if inv, found := d.inverters[bi.mac_address]; found {
				if inv.configured {
					if inv.detected {
						// already known and detected, just mark as online
						inv.online = true
					} else {
						// first detection of configured inverter
						inv.detected = true
						inv.online = true
						inv.basicInfo = bi
					}
				} else {
					// already detected, but not configured by user
					inv.online = true
				}
				d.inverters[bi.mac_address] = inv
			} else {
				log.Printf("INFO: Discovered UNCONFIGURED Inverter with MAC address: %s and unit name set to: %s\n",
					bi.mac_address, bi.name)
				var i InverterT
				i.configured = false
				i.detected = true
				i.online = true
				i.basicInfo = bi
				d.inverters[bi.mac_address] = i
			}
		}
	} // end loop
}

func (d *DaikinT) httpGet(url string) (ba []byte, err error) {
	resp, err := d.httpReqClient.Get(url)
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()
	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		return nil, err
	}
	return body, nil
}

func (d *DaikinT) fetchAndPublishBasicInfo(inverter InverterT) {
	var BI BasicInfoT
	ba, err := d.httpGet(inverter.basicInfo.address + getBasicInfo)
	if err != nil {
		log.Printf("WARNING: Basic Information request failed for %s with %s\n",
			inverter.friendlyName, err)
		// TODO mark offline?
	} else {
		BI = parseBasicInfo(ba)
		subtopic := "/" + inverter.friendlyName + "/basic"
		JSON := basicInfoToJSON(BI)
		// if BI.retOK {
		d.mq.PublishChan <- mqtt.MessageT{Subtopic: subtopic, Qos: 0, Retained: false, Payload: JSON}
		// } else {
		// 	log.Printf("WARNING: Unit %s did not return valid Sensor Info\n", inverter.friendlyName)
		// }
	}
}

func (d *DaikinT) fetchAndPublishSensorInfo(inverter InverterT) {
	var SI SensorInfoT
	ba, err := d.httpGet(inverter.basicInfo.address + getSensorInfo)
	if err != nil {
		log.Printf("WARNING: Sensor Information request failed for %s with %s\n",
			inverter.friendlyName, err)
		// TODO mark offline?
	} else {
		SI = parseSensorInfo(ba)
		subtopic := "/" + inverter.friendlyName + "/sensors"
		JSON := sensorInfoToJSON(SI)
		if SI.retOK {
			d.mq.PublishChan <- mqtt.MessageT{Subtopic: subtopic, Qos: 0, Retained: false, Payload: JSON}
		} else {
			log.Printf("WARNING: Unit %s did not return valid Sensor Info\n", inverter.friendlyName)
		}
	}
}

func (d *DaikinT) fetchControlInfo(inverter InverterT) (CI ControlInfoT, ok bool) {
	ok = true
	ba, err := d.httpGet(inverter.basicInfo.address + getControlInfo)
	if err != nil {
		log.Printf("WARNING: Control Information request failed for %s with %s\n",
			inverter.friendlyName, err)
		// TODO mark offline?
		ok = false
	} else {
		CI = parseControlInfo(ba)
		ok = CI.retOK
	}
	return CI, ok
}

func (d *DaikinT) fetchAndPublishControlInfo(inverter InverterT) {
	CI, ok := d.fetchControlInfo(inverter)
	if ok {
		subtopic := "/" + inverter.friendlyName + "/controls"
		JSON := controlInfoToJSON(CI)
		d.mq.PublishChan <- mqtt.MessageT{Subtopic: subtopic, Qos: 0, Retained: false, Payload: JSON}
	}
}

func modeIndex(mode string) int {
	for index, c := range modeArr {
		if c == mode {
			return index
		}
	}
	log.Printf("WARNING: Invalid Mode string: %s\n", mode)
	return -1
}

// sendControlInfo fetches the current CI of a unit, then sends it back with
// any changes
func (d *DaikinT) sendControlInfo(inverter InverterT, CIjson []byte) {
	var userData map[string]interface{}
	err := json.Unmarshal(CIjson, &userData)
	if err != nil {
		log.Printf("WARNING: Invalid Control data supplied for 'set' command: %s\n", string(CIjson))
	} else {
		oldCI, ok := d.fetchControlInfo(inverter)
		if ok {
			newCI := oldCI
			if newPower, found := userData["power"]; found {
				newCI.power = newPower.(bool)
			}
			if newMode, found := userData["mode"]; found {
				newCI.mode = newMode.(string)
			}
			if newStemp, found := userData["set_temp"]; found {
				newCI.setTemp = int(newStemp.(float64))
			}
			if newShum, found := userData["set_dumidity"]; found {
				newCI.setHumidity = int(newShum.(float64))
			}
			if newFrate, found := userData["fan_rate"]; found {
				newCI.fanRate = newFrate.(string)
			}
			if newSweep, found := userData["fan_sweep"]; found {
				newCI.fanSweep = newSweep.(string)
			}

			var pow, mode, stemp, shum, frate, fdir string
			if newCI.power {
				pow = "1"
			} else {
				pow = "0"
			}
			mode = strconv.Itoa(modeIndex(newCI.mode))
			stemp = strconv.Itoa(newCI.setTemp)
			shum = strconv.Itoa(newCI.setHumidity)
			frate = inverseFanRateMap[newCI.fanRate]
			fdir = strconv.Itoa(inverseFanDirMap[newCI.fanRate])

			args := fmt.Sprintf(setControlFmt, pow, mode, stemp, frate, fdir, shum)
			_, err := d.httpGet(inverter.basicInfo.address + setControlInfo + args)
			if err != nil {
				log.Printf("WARNING: Error sending control command %v\v", err)
			}
		}
	}
}

// MonitorUnits should be run as a Goroutine
func (d *DaikinT) MonitorUnits() {
	// var SI SensorInfoT
	for { // executed every conf.Update_Period seconds
		for _, inv := range d.inverters {
			// loop over all configured inverters that are online
			if (inv.configured) && (inv.online) {
				d.fetchAndPublishSensorInfo(inv)
				// experimental inter-request pause...
				time.Sleep(150 * time.Millisecond) // TODO parm?
				d.fetchAndPublishControlInfo(inv)
			}
		}
		time.Sleep(time.Duration(d.conf.Update_Period) * time.Second)
	}
}

// MonitorRequests handles 'get' and 'set' requests coming in via MQTT
// It can be run as either the final loop, or as a Goroutine.
func (d *DaikinT) MonitorRequests() {
	reqChan := d.mq.SubscribeToSubTopic("/+/+/+") // subscribe to get and set subtopics for all units
	for {
		req := <-reqChan
		//     	payload := string(req.Payload.([]uint8))
		// topic format is base_topic/friendly_name/set|get/info_type
		topicSlice := strings.Split(req.Subtopic, "/")
		friendlyName := topicSlice[1]
		command := topicSlice[2]
		infoType := topicSlice[3]
		log.Printf("DEBUG: Got command %s, %s for unit %s\n", command, infoType, friendlyName)
		if mac, found := d.invertersByName[friendlyName]; found {
			switch command {
			case "get":
				switch infoType {
				case "basic":
					d.fetchAndPublishBasicInfo(d.inverters[mac])
				case "controls":
					d.fetchAndPublishControlInfo(d.inverters[mac])
				case "sensors":
					d.fetchAndPublishSensorInfo(d.inverters[mac])
				default:
					log.Printf("WARNING: Unrecognised get subtopic `%s` received via MQTT\n", infoType)
				}
			case "set":
				switch infoType {
				case "controls":
					jsonCI := req.Payload.([]byte)
					d.sendControlInfo(d.inverters[mac], jsonCI)
					time.Sleep(750 * time.Millisecond) // the unit needs time to handle the request
					d.fetchAndPublishControlInfo(d.inverters[mac])
				default:
					log.Printf("WARNING: Unrecognised set subtopic `%s` received via MQTT\n", infoType)
				}
			default:
				log.Printf("WARNING: Unrecognised command `%s` received via MQTT\n", command)
			}
		} else {
			log.Printf("WARNING: Unrecognised inverter name `%s` received via MQTT\n", friendlyName)
		}
	}
}
