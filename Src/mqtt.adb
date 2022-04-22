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

-- with Mosquitto;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

with GNAT.String_Split; use GNAT.String_Split;

package body MQTT is

    procedure Connect (Conf : in Config.MQTT_T) is
    begin

        Mosq_Handle.Initialize (ID => MQTT_ID, Clean_Sessions => False);
        Pump.Start;
        Mosq_Handle.Set_Handler (A'Unchecked_Access);

        Mosq_Handle.Connect (Host => To_String (Conf.Broker), Port => Conf.Port, Keepalive => Keepalive);

        Mosq_Handle.Subscribe (Topic => To_String (Conf.Base_Topic) & "/#");

        Mosq_Handle.Publish ( 
                           Topic => To_String (Conf.Base_Topic) & "/status", 
                           Payload => "Started", 
                           Qos => Mosquitto.Qos_0, 
                           Retain => False);

    end Connect;

    overriding procedure On_Message
     (Self    : not null access This_App_T;
      Mosq    : Handle_Ref;
      Mid     : Message_Id;
      Topic   : String;
      Payload : Ada.Streams.Stream_Element_Array;
      QoS     : QoS_Type;
      Retain  : Boolean) is
      Subtopics : Slice_Set;
    begin
        Put_Line ("DEBUG: MQTT got message in topic: " & Topic);
        Create (S => Subtopics, From => Topic, Separators => "/", Mode => Multiple);
        Put_Line ("DEBUG: Subtopic: " & Slice (Subtopics, 2));
        if Slice_Count (Subtopics) > 2 then
            if Slice (Subtopics, 3) = "get" then
                Put_Line ("DEBUG: 'get' request");
            elsif Slice (Subtopics, 3) = "set" then
                Put_Line ("DEBUG: 'set' request");
            else
                Put_Line ("DEBUG: Ignoring this message");
            end if;
        else 
            Put_Line ("DEBUG: Ignoring this message");
        end if;
    end On_Message;

    task body Pump_T is
    begin
        select 
            accept Start;
        or 
            terminate;
        end select;
        Put_Line ("DEBUG: MQTT message pump started");
        Connection.Loop_Forever;
    end Pump_T;

end MQTT;