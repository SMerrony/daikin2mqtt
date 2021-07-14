# daikin2mqtt
A software bridge between certain popular Daikin&trade; HVAC units and MQTT.

N.B. This code has been independently developed using a mixture of observed behaviour and information freely available on the Internet. 
It is neither endorsed nor supported by Daikin in any way whatsoever. You use this software entirely at your own risk.

## Description and Purpose
This project provides a two-way link between some Daikin HVAC units' WiFi interfaces and your MQTT IoT network.

Daikin2mqtt runs as a background service or *daemon* and keeps track of the availability of the HVAC units and
communicates with them mainly in response to MQTT messages.

## User Guide
See USER_GUIDE.md

## Rationale and Inspiration
The connectivity of these HVAC units is known to be rather temperamental; units seem to appear and disappear occasionally 
even on well-configured WiFi networks.  This project aims to smooth out these wrinkles as much as possible and provide a **simple to use MQTT interface that will be accessible by most home automation systems and MQTT dashboards.**

We adopt the old Unix&trade; philosophy of trying to 'do just one thing and do it well'.

We think that https://zigbee2mqtt.io does a great job of standardising its MQTT messages, 
so we try to model our message structure
on [theirs](https://www.zigbee2mqtt.io/information/mqtt_topics_and_message_structure.html).

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
