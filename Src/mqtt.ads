-- Copyright ©2022 Steve Merrony

-- Daikin2MQTT is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- Daikin2MQTT is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with Daikin2MQTT.  If not, see <https://www.gnu.org/licenses/>.

with Ada.Streams;

with Mosquitto; use Mosquitto;
-- with Mosquitto.Logging_Application;

with Config;

package MQTT is

    task type Pump_T (Connection : access Mosquitto.Handle) is
        entry Start;
    end Pump_T;

    type This_App_T is new Mosquitto.Application_Interface with null record;

    procedure Connect (Conf : in Config.MQTT_T; Verbose : in Boolean);
    -- Connect to the MQTT broker and start the message pump.

    procedure Pub (Subtopic, Payload : in String);
    -- Simple MQTT message publish using our default QoS and retention.

    overriding procedure On_Message(Self    : not null access This_App_T;
                                    Mosq    : Handle_Ref;
                                    Mid     : Message_Id;
                                    Topic   : String;
                                    Payload : Ada.Streams.Stream_Element_Array;
                                    QoS     : QoS_Type;
                                    Retain  : Boolean);

    App         : aliased This_App_T;
    MQTT_ID     : constant String := "Daikin2MQTT";
    Keepalive   : constant Duration := 30.0;

    MQTT_Conf   : Config.MQTT_T;
    Verbose     : Boolean;    
    Mosq_Handle : aliased Mosquitto.Handle;
    Pump        : Pump_T (Mosq_Handle'Access);

end MQTT;