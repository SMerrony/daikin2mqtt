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

with Ada.Calendar;
with Ada.Text_IO;           use Ada.Text_IO;
with AWS.Client, AWS.Response, AWS.Messages;

with GNAT.Calendar.Time_IO;
with GNAT.String_Split;

-- with GNATCOLL.JSON;

with MQTT;

package body Daikin is

    function Time_HHMMSS return String is
        Pic : constant GNAT.Calendar.Time_IO.Picture_String := "%H:%M:%S";
    begin
        return GNAT.Calendar.Time_IO.Image (Ada.Calendar.Clock, Pic);
    end Time_HHMMSS;

    function Parse_Basic_Info (Buffer : in String) return Basic_Info_T is
        use GNAT.String_Split;
        KV_Pairs : Slice_Set;
        KVs      : Slice_Set;
        BI       : Basic_Info_T;
    begin
        BI.Timestamp := Time_HHMMSS;
        Create (S => KV_Pairs, From => Buffer, Separators => ",", Mode => Multiple);
        for Ix in 1 .. Slice_Count (KV_Pairs) loop
            -- Put_Line ("DEBUG: Parse_Basic_Info examining: " & Slice(KV_Pairs,Ix));
            Create (S => KVs, From => Slice(KV_Pairs,Ix), Separators => "=", Mode => Multiple);
            declare
                Key : constant String := Slice (KVs, 1);
                Val : constant String := Slice (KVs, 2);
            begin
                if    Key = "ver"      then BI.Firmware_Version := +Val;
                elsif Key = "rev"      then BI.Adaptor_Version := +Val;
                elsif Key = "pow"      then BI.Powered_On := (Val = "1");
                elsif Key = "err"      then BI.Error_Code := Integer'Value(Val);
                elsif Key = "name"     then BI.Name := +Val; -- TODO decode this URL-encoded string
                elsif Key = "mac"      then BI.Mac_Address := +Val;
                elsif Key = "adp_kind" then BI.Adaptor_Type := Integer'Value(Val);
                elsif Key = "led"      then BI.Adaptor_Led := (Val = "1");
                elsif Key = "en_hol"   then BI.Holiday_Mode := (Val = "1");
                elsif Key = "en_grp"   then BI.Group_Mode := (Val = "1");
                elsif Key = "grp_name" then BI.Group_Name := +Val;
                end if;
            end;
        end loop;
        return BI;
    end Parse_Basic_Info;

    function Parse_Control_Info (Buffer : in String) return Control_Info_T is
        -- Decode the Control Information response from an inverter which looks like this:
        -- ret=OK,pow=1,mode=4,adv=,stemp=19.0,shum=0,dt1=25.0,dt2=M,dt3=25.0,dt4=19.0,dt5=19.0,
        -- dt7=25.0,dh1=AUTO,dh2=50,dh3=0,dh4=0,dh5=0,dh7=AUTO,dhh=50,b_mode=4,b_stemp=19.0,b_shum=0,
        -- alert=255,f_rate=A,f_dir=0,b_f_rate=A,b_f_dir=0,dfr1=5,dfr2=5,dfr3=5,dfr4=A,dfr5=A,dfr6=5,
        -- dfr7=5,dfrh=5,dfd1=0,dfd2=0,dfd3=0,dfd4=0,dfd5=0,dfd6=0,dfd7=0,dfdh=0
        use GNAT.String_Split;
        KV_Pairs : Slice_Set;
        KVs      : Slice_Set;
        CI       : Control_Info_T;
    begin
        CI.Timestamp := Time_HHMMSS;
        Create (S => KV_Pairs, From => Buffer, Separators => ",", Mode => Single);
        -- Put_Line ("DEBUG: Parse_Control_Info - No. of K/V pairs:" & Slice_Count (KV_Pairs)'Image);
        for Ix in 1 .. Slice_Count (KV_Pairs) loop 
            Create (S => KVs, From => Slice(KV_Pairs,Ix), Separators => "=", Mode =>Single);
            declare
                Key : constant String := Slice (KVs, 1);
                Val : constant String := Slice (KVs, 2);
            begin
                -- Put_Line ("DEBUG: Parse_Control_Info - Key: " & Key);
                -- Put_Line ("DEBUG: Parse_Control_Info - Val: " & Val);
                if    Key = "ret"   then CI.Ret_OK := (Val = "OK");
                elsif Key = "pow"   then CI.Power  := (Val = "1");
                elsif Key = "mode"  then 
                    CI.Mode   := Natural'Value(Val);
                elsif Key = "stemp" then 
                    -- stemp is returned as a float, but is always integral, 2 exceptions...
                    if Val = "M" or Val = "--" then 
                        CI.Set_Temp := 0;
                    else
                        CI.Set_Temp := Natural(Float'Value(Val));
                    end if;
                elsif Key = "shum" then
                    if Val = "--" then
                        CI.Set_Humidity := 0;
                    else
                        CI.Set_Humidity := Natural(Float'Value(Val));
                    end if;
                elsif Key = "f_rate" then 
                    CI.Fan_Rate  := Val(Val'First);
                elsif Key = "f_dir"  then 
                    CI.Fan_Sweep := Natural'Value(Val);
                -- else
                --     Put_Line ("DEBUG: Ignoring CI Key: " & Key);
                end if;
            end;
        end loop;
        return CI;
    end Parse_Control_Info;

    function Parse_Sensor_Info (Buffer : in String) return Sensor_Info_T is
        -- Inverter returns: ret=OK,htemp=19.0,hhum=-,otemp=14.0,err=0,cmpfreq=50
        use GNAT.String_Split;
        KV_Pairs : Slice_Set;
        KVs      : Slice_Set;
        SI       : Sensor_Info_T;
    begin
        SI.Timestamp := Time_HHMMSS;
        Create (S => KV_Pairs, From => Buffer, Separators => ",", Mode => Multiple);
        for Ix in 1 .. Slice_Count (KV_Pairs) loop
            -- Put_Line ("DEBUG: Parse_Sensor_Info examining: " & Slice(KV_Pairs,Ix));
            Create (S => KVs, From => Slice(KV_Pairs,Ix), Separators => "=", Mode => Multiple);
            declare
                Key : constant String := Slice (KVs, 1);
                Val : constant String := Slice (KVs, 2);
            begin
                if    Key = "ret"   then SI.Ret_OK := (Val = "OK");
                elsif Key = "htemp" then SI.Unit_Temp := One_DP'Value(Val);
                elsif Key = "hhum"  then
                    if Val = "-" then   
                        SI.Unit_Humidity := 0;
                    else
                        SI.Unit_Humidity := Integer'Value(Val);
                    end if;
                elsif Key = "otemp" then
                    if Val = "-" then 
                        SI.Ext_Temp := -99.0;
                    else
                        SI.Ext_Temp := One_DP'Value(Val);
                    end if;
                elsif Key = "err"   then SI.Error_Code := Natural'Value(Val);
                end if;
            end;
        end loop;
        return SI;
    end Parse_Sensor_Info;

    procedure Fetch_Basic_Info (IP_Addr : in String; BI : out Basic_Info_T; OK : out Boolean) is
        Resp   : AWS.Response.Data;
        Status : AWS.Messages.Status_Code;
        Query  : constant String := "http://" & IP_addr & Get_Basic_Info;
    begin
        OK := True;
        Resp   := AWS.Client.Get (URL => Query, Timeouts => AWS.Client.Timeouts(Each => 1.0));
        Status := AWS.Response.Status_Code (Resp);
        if Status not in AWS.Messages.Success then
            Put_Line ("WARNING: Basic Information request failed with error: " & Status'Image & 
                      " from HTTP query: " & Query);
            OK := False;
        else
            declare 
                Resp_Body : constant String := AWS.Response.Message_Body (Resp);
            begin
                Put_Line ("DEBUG: Fetch_Basic_Info got response: " & Resp_Body);
                BI := Parse_Basic_Info (Resp_Body);
            end;
        end if;
    end Fetch_Basic_Info;

    procedure Fetch_Control_Info (IP_Addr : in String; CI : out Control_Info_T; OK : out Boolean) is
        Resp   : AWS.Response.Data;
        Status : AWS.Messages.Status_Code;
        Query  : constant String := "http://" & IP_addr & Get_Control_Info;
    begin
        OK := True;
        Resp   := AWS.Client.Get (URL => Query, Timeouts => AWS.Client.Timeouts(Each => 1.0));
        Status := AWS.Response.Status_Code (Resp);
        if Status not in AWS.Messages.Success then
            Put_Line ("WARNING: Control Information request failed with error: " & Status'Image & 
                      " from HTTP query: " & Query);
            OK := False;
        else
            declare 
                Resp_Body : constant String := AWS.Response.Message_Body (Resp);
            begin
                CI := Parse_Control_Info (Resp_Body);
            end;
        end if;
    end Fetch_Control_Info;

    procedure Fetch_Sensor_Info (IP_Addr : in String; SI : out Sensor_Info_T; OK : out Boolean) is
        Resp   : AWS.Response.Data;
        Status : AWS.Messages.Status_Code;
        Query  : constant String := "http://" & IP_addr & Get_Sensor_Info;
    begin
        OK := True;
        Resp   := AWS.Client.Get (URL => Query, Timeouts => AWS.Client.Timeouts(Each => 1.0));
        Status := AWS.Response.Status_Code (Resp);
        if Status not in AWS.Messages.Success then
            Put_Line ("WARNING: Sensor Information request failed with error: " & Status'Image & 
                      " from HTTP query: " & Query);
            OK := False;
        else
            declare 
                Resp_Body : constant String := AWS.Response.Message_Body (Resp);
            begin
                SI := Parse_Sensor_Info (Resp_Body);
            end;
        end if;
    end Fetch_Sensor_Info;

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

    function CI_To_JSON (CI : in Control_Info_T) return String is
        Tmp_Str : Unbounded_String;
    begin
        Tmp_Str := +"{ ""power"": " & Bool_To_JSON (CI.Power);
        Tmp_Str := Tmp_Str & ", ""mode"": """ & Mode_Arr(CI.Mode) & """";
        Tmp_Str := Tmp_Str & ", ""set_temp"": "  & CI.Set_Temp'Image;
        Tmp_Str := Tmp_Str & ", ""Set_humidity"":" & CI.Set_Humidity'Image;
        Tmp_Str := Tmp_Str & ", ""fan-rate"": """ & Fan_Rate_To_String(CI.Fan_Rate) & """";
        Tmp_Str := Tmp_Str & ", ""fan-sweep"": """ & Fan_Dir_Arr(CI.Fan_Sweep) & """";
        Tmp_Str := Tmp_Str & ", ""timestamp"": """ & CI.Timestamp & """";
        Tmp_Str := Tmp_Str & " }";
        return To_String (Tmp_Str);
    end CI_To_JSON;

    function SI_To_JSON (SI : in Sensor_Info_T) return String is
        Tmp_Str : Unbounded_String;
    begin
        Tmp_Str := +"{ ""unit_temp"": " & SI.Unit_Temp'Image;
        Tmp_Str := Tmp_Str & ", ""unit_humidity"":" & SI.Unit_Humidity'Image;
        Tmp_Str := Tmp_Str & ", ""ext_temp"": "  & SI.Ext_Temp'Image;
        Tmp_Str := Tmp_Str & ", ""error"":" & SI.Error_Code'Image;
        Tmp_Str := Tmp_Str & ", ""timestamp"": """ & SI.Timestamp & """";
        Tmp_Str := Tmp_Str & " }";
        return To_String (Tmp_Str);
    end SI_To_JSON;

    task body Monitor_Units is
        Period     : Duration;
        Monitoring : Boolean;
        IP_Addr    : Unbounded_String;
        SI         : Sensor_Info_T;
        CI         : Control_Info_T;
        OK         : Boolean;
    begin
        accept Start (Period_S : in Duration) do
            Period     := Period_S;
            Monitoring := True;
        end Start;
        Put_Line ("INFO: Monitor_Units task started");
        while Monitoring loop
            declare
                Inv_Arr  : constant Inverter_Name_Arr_T := State.Get_Online_Inverters;
                Inv_Stat : Inverter_Status_T;
            begin
                for I_Name of Inv_Arr loop
                    Put_Line ("DEBUG: Monitor_Units fetching status of: " & To_String (I_Name));
                    Inv_Stat := State.Get_Inverter_Status (I_Name);

                    IP_Addr := Inv_Stat.IP_Addr;
                    Put_Line ("DEBUG: ... Monitor_Units fetching Sensor Info");
                    Fetch_Sensor_Info (IP_Addr => To_String(IP_Addr), SI => SI , OK => OK);
                    if OK then
                        State.Set_Sensor_Info (I_Name, SI);
                        MQTT.Pub ("/" & To_String(I_Name) & "/sensors", SI_To_JSON (SI));
                    end if;
                    delay 0.15;
                    Put_Line ("DEBUG: ... Monitor_Units fetching Control Info");
                    Fetch_Control_Info (IP_Addr => To_String(IP_Addr), CI => CI, OK => OK);
                    if OK then
                        State.Set_Control_Info (I_Name, CI);
                        MQTT.Pub ("/" & To_String(I_Name) & "/controls", CI_To_JSON (CI));
                    end if;
                    Put_Line ("DEBUG: ... Monitor_Units done for this unit");
                end loop;
            end;

            select
                delay Period;
            or
                accept Stop;
                    Monitoring := False;
            end select;     

        end loop;
        Put_Line ("INFO: Monitor_Units task has exited");
    end Monitor_Units;

    protected body State is

        procedure Init (Conf : in Config.Daikin_T; Inv_Conf : in Config.Inverters_T; Verbose : in Boolean) is
            Inverter_Status : Inverter_Status_T;
            I               : Config.Inverter_T;
            IC              : Status_Maps.Cursor;
            OK              : Boolean;
        begin
            Daikin_Conf := Conf;
            for Ix in 1 .. Config.Inverter_Count loop
                I := Inv_Conf(Ix);
                if Status_Maps.Contains (Inverter_Statuses, I.Friendly_Name) then
                    raise Config.Duplicate_Configuration with "configured multiple times: " & To_String (I.Friendly_Name);
                end if;
                Inverter_Status.Use_IP_Addr := I.Use_IP_Addr;
                Inverter_Status.MAC_Addr    := I.MAC_Addr;
                Inverter_Status.IP_Addr     := I.IP_Addr;
                Inverter_Status.Configured  := True;
                Inverter_Status.Detected    := False;
                Inverter_Status.Online      := False;
                Status_Maps.Insert (Container => Inverter_Statuses, 
                                      Key => I.Friendly_Name, 
                                      New_Item => Inverter_Status, 
                                      Position => IC, 
                                      Inserted => OK);
                if not OK then
                    Put_Line ("ERROR: Could not configure inverter: " & To_String(I.Friendly_Name));
                elsif Verbose then
                    Put_Line ("DEBUG: Configured inverter: " & To_String(I.Friendly_Name));
                end if;
            end loop;
            Put_Line ("INFO:" & Status_Maps.Length (Inverter_Statuses)'Image & " Inverters configured");
        end Init;

        procedure Discover_UDP is
        begin
            if Config.Inverters_MAC = 0 then
                return;
            end if;

        end Discover_UDP;

        procedure Discover_IP is
            BI : Basic_Info_T;
            OK : Boolean;
        begin
            if Config.Inverters_IP = 0 then
                return;
            end if;

            for I in Inverter_Statuses.Iterate loop
                if Status_Maps.Element (I).Use_IP_Addr then
                    Put_Line ("DEBUG: Would check inverter: " & To_String (Status_Maps.Key (I)));
                    Fetch_Basic_Info (IP_Addr => To_String (Status_Maps.Element(I).IP_Addr), 
                                      BI => BI, 
                                      OK => OK);
                    if OK then
                        Set_Inverter_Online (Status_Maps.Key (I), True);
                    end if;
                end if;
            end loop;

        end Discover_IP;

        function Get_Inverter_Status (F_Name : in Unbounded_String) return Inverter_Status_T is
            Inv : Inverter_Status_T;
        begin
            if Inverter_Statuses.Contains (F_Name) then
                Inv := Inverter_Statuses.Element (Key => F_Name);
            else
                raise Unknown_Inverter_Name with To_String (F_Name);
            end if;
            return Inv;
        end Get_Inverter_Status;

        function Get_Online_Inverters return Inverter_Name_Arr_T is
            Online_Count : Natural := 0;
        begin
            for I in Inverter_Statuses.Iterate loop
                if Status_Maps.Element(I).Online then
                    Online_Count := Online_Count + 1;
                end if;
            end loop;
            Put_Line ("DEBUG: Get_Online_Inverters will return" & Online_Count'Image & " inverters");
            declare
                INA : Inverter_Name_Arr_T (1 .. Online_Count);
                Ix  : Natural := 0;
            begin
                for I in Inverter_Statuses.Iterate loop
                    if Status_Maps.Element(I).Online then
                        Ix := Ix + 1;
                        INA(Ix) := Status_Maps.Key(I);
                    end if;
                end loop;
                return INA;
            end;
        end Get_Online_Inverters;

        procedure Set_Control_Info (F_Name : in Unbounded_String; CI : in Control_Info_T) is
        begin
            if Inverter_Controls.Contains (F_Name) then
                Control_Maps.Replace_Element (Container => Inverter_Controls, 
                                              Position  => Control_Maps.Find (Inverter_Controls, F_Name), 
                                              New_Item  => CI);
            else
                Control_Maps.Insert (Container => Inverter_Controls, 
                                     Key       => F_Name, 
                                     New_Item  => CI);
            end if;
        end Set_Control_Info;

        procedure Set_Sensor_Info (F_Name : in Unbounded_String; SI : in Sensor_Info_T) is
        begin
            if Inverter_Sensors.Contains (F_Name) then
                Sensor_Maps.Replace_Element (Container => Inverter_Sensors, 
                                             Position  => Sensor_Maps.Find (Inverter_Sensors, F_Name), 
                                             New_Item  => SI);
            else
                Sensor_Maps.Insert (Container => Inverter_Sensors, 
                                    Key       => F_Name, 
                                    New_Item  => SI);
            end if;
        end Set_Sensor_Info;

        procedure Set_Inverter_Online (F_Name : in Unbounded_String; Online : in Boolean) is
            Inv_Stat : Inverter_Status_T := Get_Inverter_Status (F_Name);
        begin
            Inv_Stat.Online := Online;
            Status_Maps.Replace_Element (Container => Inverter_Statuses, 
                                         Position  => Status_Maps.Find (Inverter_Statuses, F_Name),
                                         New_Item  => Inv_Stat);
        end Set_Inverter_Online;


    end State;

end Daikin;