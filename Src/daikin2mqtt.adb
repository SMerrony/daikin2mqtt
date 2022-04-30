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

-- Ada standard packages...
with Ada.Command_Line;        use Ada.Command_Line;
with Ada.Directories;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Text_IO;             use Ada.Text_IO;

-- Program packages...
with Config;
with Daikin;
-- with MQTT;

procedure Daikin2MQTT is

    App_SemVer : constant String := "v0.2.0"; -- TODO Update SemVer for every release

    Arg_Ix     : Natural := 1;
    Config_Arg : Unbounded_String := Null_Unbounded_String;
    Verbose    : Boolean := False;

begin

    while Arg_Ix <= Argument_Count loop
        if Argument (Arg_Ix) = "-V" or Argument (Arg_Ix) = "-version" then
            Ada.Text_IO.Put_Line ("daikin2mqtt version " & App_SemVer);
            return;
        elsif Argument (Arg_Ix) = "-v" or Argument (Arg_Ix) = "-verbose" then
            Verbose := True;
        elsif Argument (Arg_Ix) = "-h" or Argument (Arg_Ix) = "-help "then
            Put_Line ("Usage of daikin2mqtt:");
            Put_Line ("  -config <config-file>  Configuration file for daikin2mqtt (required)");
            Put_Line ("  -h | -help             Show this help");
            Put_Line ("  -V | -version          Show the version of daikin2mqtt and exit");
            Put_Line ("  -v | -verbose          Show lots of detail when running");
            return;
        elsif Argument (Arg_Ix) = "-config" then
            Config_Arg := To_Unbounded_String (Argument (Arg_Ix + 1));
            Arg_Ix := Arg_Ix + 1;
        end if;
        Arg_Ix := Arg_Ix + 1;
    end loop;

    if Config_Arg = Null_Unbounded_String then
        Put_Line ("ERROR: You must specify a configuration file.  Use '-h' for help.");
        return;
    end if;

    if not Ada.Directories.Exists (To_String (Config_Arg)) then
        Put_Line ("ERROR: Configuration file does not exist.");
        return;
    end if;

    Config.Load_Config_File (To_String (Config_Arg), Verbose);

    Daikin.MQTT_Connect (Config.MQTT);

    Daikin.State.Init (Conf => Config.Daikin, Inv_Conf => Config.Inverters, Verbose => Verbose);

    Daikin.Monitor_Units.Start (Period_S => Config.Daikin.Update_Period_S);

end Daikin2MQTT;