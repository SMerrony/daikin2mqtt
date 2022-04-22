-- Copyright ©2022 Steve Merrony

-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
-- The above copyright notice and this permission notice shall be included in
-- all copies or substantial portions of the Software.

-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
-- THE SOFTWARE.

with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;

package Config is

    type Daikin_T is record
        Discovery_Timeout_S  : Positive;
        Rediscovery_Period_M : Positive;
        Update_Period_S      : Positive;
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
        Use_IP_Addr   : Boolean;
        MAC_Addr      : Unbounded_String;
        IP_Addr       : Unbounded_String;
    end record;

    Max_Inverters : constant Positive := 24;
    -- This value is somewhat arbitrary, it's mainly set this low to ensure that
    -- the config parsing doesn't go into a loop.

    type Inverters_T is array (1 .. Max_Inverters) of Inverter_T;

    Inverters : Inverters_T;
    -- Array of configured inverter names and addresses.

    Inverter_Count : Positive;
    -- The number of inverters that are configured,
    -- i.e. present in the configuration file.

    procedure Load_Config_File (Filename : in String; Verbose : in Boolean);

    Could_Not_Parse,
    Incomplete_Configuration,
    Unknown_Configuration_Item : exception;

end Config;