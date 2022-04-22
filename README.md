# daikin2mqtt
A software bridge between certain popular Daikin&trade; HVAC units and MQTT.  Monitor and control 
your HVAC from any MQTT-connected dashboard or home automation system.

N.B. This code has been independently developed using a mixture of observed behaviour and information freely available on the Internet. 
It is neither endorsed nor supported by Daikin in any way whatsoever. You use this software entirely at your own risk.

## Description and Purpose
This project provides a two-way link between some Daikin HVAC units' WiFi interfaces and your MQTT IoT network.

*daikin2mqtt* runs as a background service or *daemon* to keep track of the availability of the HVAC units and provide an easy to use MQTT interface with which you can monitor and control the system.

## User Guide
See [our Wiki](https://github.com/SMerrony/daikin2mqtt/wiki).

## Rationale and Inspiration
The connectivity of these HVAC units is known to be rather temperamental; units seem to appear and disappear occasionally 
even on well-configured WiFi networks.  This project aims to smooth out these wrinkles as much as possible and provide a **simple to use MQTT interface that will be accessible by most home automation systems and MQTT dashboards.**

We adopt the old Unix&trade; philosophy of trying to 'do just one thing and do it well'.

We think that https://zigbee2mqtt.io does a great job of standardising its MQTT messages, 
so our message structure is inspired by 
[theirs](https://www.zigbee2mqtt.io/information/mqtt_topics_and_message_structure.html).

The predecessor to this project was the [Daikin Integration](https://github.com/SMerrony/aghast/blob/main/docs/Daikin.md) in the https://github.com/SMerrony/aghast project.  In turn, that project was inspired by
https://github.com/ael-code/daikin-control .

## Supported Devices
Note the the Integration does not *directly* support specific HVAC units, 
rather it communicates with the network adapters that may be built-in, or added-to, 
various 'inverters' (as Daikin refers to their interior units).

* BRP069B41 - the Integration has been extensively tested with the BRP069B41 adapters.
We would expect all of the BRP069B41/2/3/4/5 adapters to work.

* BRP069A41/2/3/4/5 - we believe that the previous models, the BRP069A41/2/3/4/5 should also work.

* BRP072A42 - we expect that the BRP072A42 adapter would also work.

We do _not_ expect the BRP072Cnn or SKYFi units to work.

Please let us know if you can add to the above information.

## Licence and Copyright
Copyright ©2021,2022 Stephen Merrony

Daikin2MQTT is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Daikin2MQTT is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Daikin2MQTT.  If not, see <https://www.gnu.org/licenses/>.
