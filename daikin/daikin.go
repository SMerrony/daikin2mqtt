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
	"log"
	"net"
	"net/url"
	"strconv"
	"strings"
	"time"
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
}

type ControlInfoT struct {
	retOK       bool
	power       bool
	mode        int
	setTemp     int
	setHumidity int
	fanRate     int
	fanSweep    int
}

type SensorInfoT struct {
	retOK        bool
	unitTemp     float32
	unitHumidity float32
	extTemp      float32
	errorCode    int
}

type InverterT struct {
	friendlyName string
	configured,
	detected,
	online bool
	basicInfo   BasicInfoT
	SensorInfo  SensorInfoT
	controlInfo ControlInfoT
}

type DaikinT struct {
	conf      DaikinConfT
	inverters map[string]InverterT
}

func Create(dConf DaikinConfT, iConf []InverterConfT) (d DaikinT) {
	d.conf = dConf
	d.inverters = make(map[string]InverterT)
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
	}
	log.Printf("INFO: %d Inverters found in configuration", len(d.inverters))
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
	return BI
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
			bi.address = net.JoinHostPort(host, "80")
			bi = parseBasicInfo(buf[:n])

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
