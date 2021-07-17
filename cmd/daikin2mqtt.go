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

package main

import (
	"flag"
	"log"
	"os"
	"runtime"

	"github.com/SMerrony/daikin2mqtt/daikin"
	"github.com/SMerrony/daikin2mqtt/mqtt"
	"github.com/pelletier/go-toml"
)

type ConfigT struct {
	Mqtt     mqtt.MqttConfT
	Daikin   daikin.DaikinConfT
	Inverter []daikin.InverterConfT
}

const semVer = "v0.1.0" // TODO Update semVer const on each release!

func main() {

	var (
		configFlag  = flag.String("conf", "", "directory containing the configuration file")
		versionFlag = flag.Bool("version", false, "display version number and exit")
	)

	var config ConfigT

	flag.Parse()
	if *versionFlag {
		log.Printf("daikin2mqtt version %s built with %s\n", semVer, runtime.Version())
		return
	}

	if *configFlag == "" {
		log.Fatalln("ERROR: You must supply a -conf option")
	}
	confBytes, err := os.ReadFile(*configFlag)
	if err != nil {
		log.Fatal(err)
	}
	err = toml.Unmarshal(confBytes, &config)
	if err != nil {
		log.Fatalf("ERROR: Could not load config due to %s\n", err.Error())
	}
	log.Printf("DEBUG: Configuration loaded, MQTT Broker is %s\n", config.Mqtt.Broker)

	var mq mqtt.MQTT_T

	mq.Start(config.Mqtt)

	dk := daikin.Create(config.Daikin, config.Inverter, mq)
	dk.Discover()

	go dk.MonitorUnits()

	go dk.DiscoveryLoop()

	dk.MonitorRequests()

	// time.Sleep(5 * time.Second)

}
