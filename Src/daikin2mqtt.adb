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

-- Ada standard packages...
with Ada.Command_Line;        use Ada.Command_Line;
with Ada.Directories;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Text_IO;             use Ada.Text_IO;

-- Program packages...
with Config;

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


end Daikin2MQTT;