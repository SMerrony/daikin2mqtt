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

with Ada.Strings.Unbounded;         use Ada.Strings.Unbounded;

package body Daikin_JSON is

    function Bool_To_JSON (ItIs : in Boolean) return String is
    begin
        if ItIs then return "true"; else return "false"; end if;
    end Bool_To_JSON;

    function Fan_Rate_To_String (FR : in Character) return String is
    begin
        case FR is
            when 'A' => return "AUTO";
            when 'B' => return "SILENT";
            when '3' => return "LEVEL_1";
            when '4' => return "LEVEL_2";
            when '5' => return "LEVEL_3";
            when '6' => return "LEVEL_4";
            when '7' => return "LEVEL_5";    
            when others =>
                raise Unknown_Fan_Rate with "" & FR;
        end case;
    end Fan_Rate_To_String;

    function BI_To_JSON (BI : in Basic_Info_T) return String is
        Tmp_Str : Unbounded_String;
    begin
        Tmp_Str := +"{ ""firmware_version"": """    & BI.Firmware_Version & """";
        Tmp_Str := Tmp_Str & ", ""power"": "        & Bool_To_JSON (BI.Powered_On);
        Tmp_Str := Tmp_Str & ", ""error"": "        & BI.Error_Code'Image;
        Tmp_Str := Tmp_Str & ", ""adaptor_led"": "  & Bool_To_JSON (BI.Adaptor_Led);
        Tmp_Str := Tmp_Str & ", ""holiday_mode"": " & Bool_To_JSON (BI.Holiday_Mode);
        Tmp_Str := Tmp_Str & ", ""group_mode"": "   & Bool_To_JSON (BI.Group_Mode);
        Tmp_Str := Tmp_Str & ", ""group_name"": """ & BI.Group_Name & """";
        Tmp_Str := Tmp_Str & ", ""timestamp"": """  & BI.Timestamp & """";
        Tmp_Str := Tmp_Str & " }";
        return To_String (Tmp_Str);
    end BI_To_JSON;

    function CI_To_JSON (CI : in Control_Info_T) return String is
        Tmp_Str : Unbounded_String;
    begin
        Tmp_Str := +"{ ""power"": "                & Bool_To_JSON (CI.Power);
        Tmp_Str := Tmp_Str & ", ""mode"": """      & Mode_Arr(CI.Mode) & """";
        Tmp_Str := Tmp_Str & ", ""set_temp"": "    & CI.Set_Temp'Image;
        Tmp_Str := Tmp_Str & ", ""Set_humidity"":" & CI.Set_Humidity'Image;
        Tmp_Str := Tmp_Str & ", ""fan-rate"": """  & Fan_Rate_To_String(CI.Fan_Rate) & """";
        Tmp_Str := Tmp_Str & ", ""fan-sweep"": """ & Fan_Sweep_Arr(CI.Fan_Sweep) & """";
        Tmp_Str := Tmp_Str & ", ""timestamp"": """ & CI.Timestamp & """";
        Tmp_Str := Tmp_Str & " }";
        return To_String (Tmp_Str);
    end CI_To_JSON;

    function SI_To_JSON (SI : in Sensor_Info_T) return String is
        Tmp_Str : Unbounded_String;
    begin
        Tmp_Str := +"{ ""unit_temp"": "             & SI.Unit_Temp'Image;
        Tmp_Str := Tmp_Str & ", ""unit_humidity"":" & SI.Unit_Humidity'Image;
        Tmp_Str := Tmp_Str & ", ""ext_temp"": "     & SI.Ext_Temp'Image;
        Tmp_Str := Tmp_Str & ", ""error"":"         & SI.Error_Code'Image;
        Tmp_Str := Tmp_Str & ", ""timestamp"": """  & SI.Timestamp & """";
        Tmp_Str := Tmp_Str & " }";
        return To_String (Tmp_Str);
    end SI_To_JSON;

end Daikin_JSON;
