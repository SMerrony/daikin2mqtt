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

with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;

package Config is

    type Daikin_T is record
        Update_Period_S      : Duration;
    end record;

    Daikin : Daikin_T;
    -- The Daikin configuration specifies various ways in which the 
    -- program will behave during a run.

    type MQTT_T is record
        Base_Topic : Unbounded_String;
        Broker     : Unbounded_String;
        Port       : Positive;
        Username   : Unbounded_String;
        Password   : Unbounded_String;        
    end record;

    MQTT : MQTT_T;
    -- The MQTT connection and topic configuration.  
    -- The Username and Password are optional.

    type Inverter_T is record
        Friendly_Name : Unbounded_String;
        IP_Addr       : Unbounded_String;
    end record;

    Max_Inverters : constant Positive := 24;
    -- This value is somewhat arbitrary, it's mainly set this low to ensure that
    -- the config parsing doesn't go into a crazy loop.

    type Inverters_T is array (1 .. Max_Inverters) of Inverter_T;

    Inverters : Inverters_T;
    -- Array of configured inverter names and addresses.

    Inverter_Count : Natural := 0;
    -- The number of inverters that are configured,
    -- i.e. present in the configuration file.

    procedure Load_Config_File (Filename : String; Verbose : Boolean);

    Could_Not_Parse,
    Duplicate_Configuration,
    Incomplete_Configuration,
    Unknown_Configuration_Item : exception;

end Config;