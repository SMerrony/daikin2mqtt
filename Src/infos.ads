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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Infos is

    function "+"(S : String) return Unbounded_String renames Ada.Strings.Unbounded.To_Unbounded_String;

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

    function Parse_Basic_Info (Buffer : in String) return Basic_Info_T;
    function Parse_Control_Info (Buffer : in String) return Control_Info_T;
    function Parse_Sensor_Info (Buffer : in String) return Sensor_Info_T;
    
end Infos;