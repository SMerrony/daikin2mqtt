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

with Ada.Text_IO; use Ada.Text_IO;

with TOML;
with TOML.File_IO;

package body Config is

    procedure Load_Config_File (Filename : String; Verbose : Boolean) is
        Toml_Parse_Result : TOML.Read_Result;
    begin
        Toml_Parse_Result := TOML.File_IO.Load_File (Filename);
        if Toml_Parse_Result.Success then
            Put_Line ("INFO: " & Filename & " loaded");
        else 
            raise Could_Not_Parse with To_String (Toml_Parse_Result.Message);
        end if;

        declare
            Top_Keys : constant TOML.Key_Array := Toml_Parse_Result.Value.Keys;
        begin
            for TK in Top_Keys'Range loop
                if Verbose then
                    Put_Line ("Configuration for " & To_String (Top_Keys(TK)) & " is...");
                end if;
                if To_String (Top_Keys(TK)) = "daikin" then
                    declare
                        Daikin_Table : constant TOML.TOML_Value := TOML.Get (Toml_Parse_Result.Value, "daikin");
                    begin
                        Daikin.Update_Period_S       := Duration(TOML.As_Integer (TOML.Get (Daikin_Table, "update_period")));
                        if Verbose then
                            Put_Line ("INFO: Update_Period (s) :" & Daikin.Update_Period_S'Image);
                        end if;
                    end;

                elsif To_String (Top_Keys(TK)) = "mqtt" then
                    declare
                        MQTT_Table : constant TOML.TOML_Value := TOML.Get (Toml_Parse_Result.Value, "mqtt");
                    begin
                        MQTT.Base_Topic := TOML.As_Unbounded_String (TOML.Get (MQTT_Table, "base_topic"));
                        MQTT.Broker     := TOML.As_Unbounded_String (TOML.Get (MQTT_Table, "broker"));
                        MQTT.Port       := Positive(TOML.As_Integer (TOML.Get (MQTT_Table, "port")));
                        MQTT.Username   := TOML.As_Unbounded_String (TOML.Get (MQTT_Table, "username"));
                        MQTT.Password   := TOML.As_Unbounded_String (TOML.Get (MQTT_Table, "password"));
                        if Verbose then
                            Put_Line ("  Base_Topic : " & To_String (MQTT.Base_Topic));
                            Put_Line ("  Broker     : " & To_String (MQTT.Broker));
                            Put_Line ("  Port       :"  & MQTT.Port'Image);
                            Put_Line ("  Username   : " & To_String (MQTT.Username));
                            Put_Line ("  Password   : " & To_String (MQTT.Password));
                        end if;
                    end;
                elsif To_String (Top_Keys(TK)) = "inverter" then
                    declare 
                        Inverter_Array : constant TOML.TOML_Value := TOML.Get (Toml_Parse_Result.Value, "inverter");
                        Invs           : constant Natural         := TOML.Length (Inverter_Array);
                        Inv            : TOML.TOML_Value;
                    begin
                        if Invs = 0 then
                            raise Incomplete_Configuration with "you must configure at least one Inverter";
                        end if;
                        for I in 1 .. Invs loop
                            Inv := TOML.Item (Inverter_Array, I);
                            Inverters(I).Friendly_Name := TOML.As_Unbounded_String (TOML.Get (Inv, "friendly_name"));
                            Inverters(I).IP_Addr       := TOML.As_Unbounded_String (TOML.Get (Inv, "ip_addr"));
                        end loop;

                        Inverter_Count := Invs;

                        if Verbose then
                            Put_Line ("  Inverters configured :" & Invs'Image);
                            for I in 1 .. Invs loop
                                Put_Line ("    " & To_String (Inverters(I).Friendly_Name));
                                Put_Line ("      " & To_String (Inverters(I).IP_Addr));
                            end loop;
                        end if;
                    end;
                else
                    raise Unknown_Configuration_Item with To_String (Top_Keys(TK));
                end if;
            end loop;
        end;

    end Load_Config_File;

end Config;