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

with Ada.Containers.Ordered_Maps;
-- with Ada.Containers.Vectors;
-- with Ada.Real_Time; 
with Ada.Streams;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Mosquitto; use Mosquitto;

with Config;
with Infos;     use Infos;

package Daikin is

    function "+"(S : String) return Unbounded_String renames Ada.Strings.Unbounded.To_Unbounded_String;

    -- these are some of the known Daikin 'end-points', we do not use them all
	Get_Basic_Info     : constant String := "/common/basic_info";
	Get_Remote_Method  : constant String := "/common/get_remote_method";
	Set_Region_Code    : constant String := "/common/set_regioncode";
    Reboot             : constant String := "/common/reboot";
	Get_Model_Info     : constant String := "/aircon/get_model_info";
	Get_Control_Info   : constant String := "/aircon/get_control_info";
	Get_Sched_Timer    : constant String := "/aircon/get_scdtimer";
	Get_Sensor_Info    : constant String := "/aircon/get_sensor_info";
	Get_Week_Power     : constant String := "/aircon/get_week_power";
	Get_Year_Power     : constant String := "/aircon/get_year_power";
	Get_Week_Power_Ext : constant String := "/aircon/get_week_power_ex";
	Get_Year_Power_Ext : constant String := "/aircon/get_year_power_ex";
	Set_Control_Info   : constant String := "/aircon/set_control_info";

    type Inverter_Status_T is record
        -- This is the 'current' status, not the preconfigured basic_info.
        -- TODO this should be extended once the original functionality is done.
        -- Friendly_Name : Unbounded_String;
        IP_Addr       : Unbounded_String;
        Detected,
        Online        : Boolean;
        Basic_Info    : Basic_Info_T; -- the Daikin info returned when we detect the unit
    end record;

    package Control_Maps is new Ada.Containers.Ordered_Maps(Unbounded_String, Control_Info_T);
    package Sensor_Maps  is new Ada.Containers.Ordered_Maps(Unbounded_String, Sensor_Info_T);
    package Status_Maps  is new Ada.Containers.Ordered_Maps(Unbounded_String, Inverter_Status_T);

    package Inverter_Xrefs is new Ada.Containers.Ordered_Maps(Unbounded_String, Unbounded_String);

--    package Inverter_Vectors is new Ada.Containers.Vectors()

    type Inverter_Name_Arr_T is array (Integer range <>) of Unbounded_String;

    protected State is

        procedure Init (Conf : Config.Daikin_T; Inv_Conf : Config.Inverters_T; Verbose : Boolean);

        function  Get_Inverter_Status (F_Name : Unbounded_String) return Inverter_Status_T;
        function  Get_Online_Inverters return Inverter_Name_Arr_T;
        procedure Set_Inverter_Online (F_Name : Unbounded_String; Online : Boolean);

        procedure Set_Control_Info (F_Name : Unbounded_String; CI : Control_Info_T);
        -- Either create a new CI record, or replace an existing one.
        procedure Set_Control_Info (IP_Addr : String; CI : Control_Info_T);

        procedure Set_Sensor_Info (F_Name : Unbounded_String; SI : Sensor_Info_T);
        -- Either create a new SI record, or replace an existing one.

        function Name_To_IP_Addr (F_Name  : String) return String;
        function IP_Addr_to_Name (IP_Addr : String) return String;

        function Is_Verbose return Boolean;

    private

        Daikin_Conf          : Config.Daikin_T;
        Inverter_Controls    : Control_Maps.Map;
        Inverter_Statuses    : Status_Maps.Map;
        Inverter_Sensors     : Sensor_Maps.Map;
        Inverters_IP_To_Name : Inverter_Xrefs.Map;
        -- Map IP address to Friendly Name
        Verbose              : Boolean;

    end State;

    task Monitor_Units is
        entry Start (Period_S : Duration);
        entry Stop;
    end Monitor_Units;

    -- MQTT stuff... --

    task type Pump_T (Connection : access Mosquitto.Handle) is
        entry Start;
    end Pump_T;

    type This_App_T is new Mosquitto.Application_Interface with null record;

    procedure MQTT_Connect (Conf : Config.MQTT_T);
    -- Connect to the MQTT broker and start the message pump.

    procedure MQTT_Pub (Subtopic, Payload : String);
    -- Simple MQTT message publish using our default QoS and retention.

    overriding procedure On_Message(Self    : not null access This_App_T;
                                    Mosq    : Handle_Ref;
                                    Mid     : Message_Id;
                                    Topic   : String;
                                    Payload : Ada.Streams.Stream_Element_Array;
                                    QoS     : QoS_Type;
                                    Retain  : Boolean);
    -- This is a callback fired whenever we receive an MQTT message

    App         : aliased This_App_T;
    MQTT_ID     : constant String := "Daikin2MQTT";
    Keepalive   : constant Duration := 30.0;

    MQTT_Conf   : Config.MQTT_T;  
    Mosq_Handle : aliased Mosquitto.Handle;
    Pump        : Pump_T (Mosq_Handle'Access);

    Unknown_Fan_Rate,
    Unknown_Inverter_Name,
    Unknown_IP_Address     : exception;

end Daikin;
