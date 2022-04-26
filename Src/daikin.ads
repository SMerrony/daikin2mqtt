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
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Config;

package Daikin is

    function "+"(S : String) return Unbounded_String renames Ada.Strings.Unbounded.To_Unbounded_String;

    -- these are all the known Daikin 'end-points', we do not use them all
	Get_Basic_Info     : constant String := "/common/basic_info";
	Get_Remote_Method  : constant String := "/common/get_remote_method";
	Set_Region_Code    : constant String := "/common/set_regioncode";
	Get_Model_Info     : constant String := "/aircon/get_model_info";
	Get_Control_Info   : constant String := "/aircon/get_control_info";
	Get_Sched_Timer    : constant String := "/aircon/get_scdtimer";
	Get_Sensor_Info    : constant String := "/aircon/get_sensor_info";
	Get_Week_Power     : constant String := "/aircon/get_week_power";
	Get_Year_Power     : constant String := "/aircon/get_year_power";
	Get_Week_Power_Ext : constant String := "/aircon/get_week_power_ex";
	Get_Year_Power_Ext : constant String := "/aircon/get_year_power_ex";
	Set_Control_Info   : constant String := "/aircon/set_control_info";

    type Mode_Arr_T is array (0 .. 7) of Unbounded_String;
    Mode_Arr : constant Mode_Arr_T := (+"AUTO",
                                       +"AUTO 1",
                                       +"DEHUMIDIFY",
                                       +"COOL",
                                       +"HEAT",
                                       +"MODE 5",
                                       +"FAN",
                                       +"AUTO 2");

    type Fan_Rate_ID_T is ('A', 'B', '3', '4', '5', '6', '7');
    type Fan_Rates_Arr_T is array (Fan_Rate_ID_T'Range) of Unbounded_String;
    Fan_Rate_Arr : constant Fan_Rates_Arr_T := (+"AUTO", 
                                                +"SILENT", 
                                                +"LEVEL_1", 
                                                +"LEVEL_2", 
                                                +"LEVEL_3", 
                                                +"LEVEL_4", 
                                                +"LEVEL_5");

    type Fan_Dir_Arr_T is array (0 .. 3) of Unbounded_String;
    Fan_Dir_Arr : constant Fan_Dir_Arr_T := (+"OFF", +"VERTICAL", +"HORIZONTAL", +"BOTH");

    type Basic_Info_T is record
        Address          : Unbounded_String;
        Mac_Address      : Unbounded_String;
        Firmware_Version : Unbounded_String;
        Adaptor_Version  : Unbounded_String; -- hex, decode unknown
        Powered_On       : Boolean;
        Error_Code       : Natural;
        Name             : Unbounded_String;
        Adaptor_Type     : Natural;
        Adaptor_Led      : Boolean;
        Holiday_Mode     : Boolean;
        Group_Mode       : Boolean;
        Group_Name       : Unbounded_String;
        Timestamp        : String(1..8);
    end record;

    type Control_Info_T is record
        Ret_OK       : Boolean;
        Power        : Boolean;             
        Mode         : Natural;   
        Set_Temp     : Natural;             
        Set_Humidity : Natural;             
        Fan_Rate     : Character;    
        Fan_Sweep    : Natural;    
        Timestamp    : String(1..8);  
    end record;

    type One_DP is delta 0.1 digits 3;

    type Sensor_Info_T is record
        Ret_OK        : Boolean;
        Unit_Temp     : One_DP;
        Unit_Humidity : Natural;
        Ext_Temp      : One_DP;
        Error_Code    : Natural;
        Timestamp     : String(1..8);
    end record;

    type Inverter_Status_T is record
        -- This is the 'current' status, not the preconfigured basic_info.
        -- TODO this should be extended once the original functionality is done.
        -- Friendly_Name : Unbounded_String;
        Use_IP_Addr   : Boolean;
        MAC_Addr      : Unbounded_String;
        IP_Addr       : Unbounded_String;
        Configured,
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

        procedure Init (Conf : in Config.Daikin_T; Inv_Conf : in Config.Inverters_T; Verbose : in Boolean);
        procedure Discover_IP;
        procedure Discover_UDP;
        function  Get_Inverter_Status (F_Name : in Unbounded_String) return Inverter_Status_T;
        function  Get_Online_Inverters return Inverter_Name_Arr_T;
        procedure Set_Inverter_Online (F_Name : in Unbounded_String; Online : in Boolean);
        procedure Set_Sensor_Info (F_Name : in Unbounded_String; SI : in Sensor_Info_T);
        -- Either create a new SI record, or replace an existing one.

    private

        Daikin_Conf       : Config.Daikin_T;
        Inverter_Statuses : Status_Maps.Map;
        Inverter_Sensors  : Sensor_Maps.Map;
        Inverters_Xref    : Inverter_Xrefs.Map;
        Verbose           : Boolean;

    end State;

    task Monitor_Units is
        entry Start (Period_S : in Duration);
        entry Stop;
    end Monitor_Units;

    Unknown_Inverter_Name : exception;

end Daikin;
