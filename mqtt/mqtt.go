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

package mqtt

import (
	"fmt"
	"log"

	mqtt "github.com/eclipse/paho.mqtt.golang"
)

const (
	mqttOutboundQueueLen = 10
	mqttInboundQueueLen  = 10
)

type MqttConfT struct {
	Base_Topic string
	Broker     string
	Port       int
	Username   string
	Password   string
}

type MessageT struct {
	Subtopic string
	Qos      byte
	Retained bool
	Payload  interface{}
}

type MQTT_T struct {
	PublishChan    chan MessageT
	conf           MqttConfT
	client         mqtt.Client
	options        *mqtt.ClientOptions
	connectHandler mqtt.OnConnectHandler
	connLostHander mqtt.ConnectionLostHandler
	pubHandler     mqtt.MessageHandler
}

func (m *MQTT_T) Start(conf MqttConfT) {
	m.options = mqtt.NewClientOptions()
	m.options.AddBroker(fmt.Sprintf("tcp://%s:%d", conf.Broker, conf.Port))
	if conf.Username != "" {
		m.options.SetUsername(conf.Username)
		m.options.SetPassword(conf.Password)
	}
	m.conf = conf
	m.options.SetClientID(conf.Base_Topic)

	m.connectHandler = func(client mqtt.Client) {
		log.Println("INFO MQTT Connected to Broker")
	}
	m.options.OnConnect = m.connectHandler

	m.connLostHander = func(client mqtt.Client, err error) {
		log.Printf("WARNING: MQTT Connection lost: %v", err)
	}
	m.options.OnConnectionLost = m.connLostHander
	m.client = mqtt.NewClient(m.options)
	if token := m.client.Connect(); token.Wait() && token.Error() != nil {
		panic(token.Error())
	}

	m.PublishChan = make(chan MessageT, mqttOutboundQueueLen)
	go m.publishViaMQTT()

	msg := MessageT{
		Subtopic: "/status",
		Qos:      0,
		Retained: false,
		Payload:  "Started",
	}
	m.PublishChan <- msg
}

// publishViaMQTT sends messages to any MQTT listeners via the configured Broker
func (m *MQTT_T) publishViaMQTT() {
	for {
		msg := <-m.PublishChan
		m.client.Publish(m.conf.Base_Topic+msg.Subtopic, msg.Qos, msg.Retained, msg.Payload)
	}
}

// SubscribeToTopic returns a channel which will receive any MQTT messages published to the
// given subtopic of the configured Base_Topic
func (m *MQTT_T) SubscribeToSubTopic(subtopic string) (c chan MessageT) {
	c = make(chan MessageT, mqttInboundQueueLen)
	m.client.Subscribe(m.conf.Base_Topic+subtopic, 1, func(client mqtt.Client, msg mqtt.Message) {
		cMsg := MessageT{msg.Topic(), msg.Qos(), msg.Retained(), msg.Payload()}
		c <- cMsg
		// log.Printf("DEBUG: MQTT subscription got topic: %s,  msg: %v\n", msg.Topic(), msg.Payload())
	})
	return c
}
