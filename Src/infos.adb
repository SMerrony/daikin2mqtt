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
with Ada.Strings;       use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
-- with Ada.Text_IO;           use Ada.Text_IO;

with Ada.Text_IO;
with GNAT.Calendar.Time_IO;
with GNAT.String_Split;

package body Infos is

    function Time_HHMMSS return String is
        Pic : constant GNAT.Calendar.Time_IO.Picture_String := "%H:%M:%S";
    begin
        return GNAT.Calendar.Time_IO.Image (Ada.Calendar.Clock, Pic);
    end Time_HHMMSS;

    function Parse_Basic_Info (Buffer : String) return Basic_Info_T is
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

    function Parse_Control_Info (Buffer : String) return Control_Info_T is
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
                    if Val = "M" or else Val = "--" then 
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

    function Parse_Sensor_Info (Buffer : String) return Sensor_Info_T is
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

    function Decode_Mode_US (Mode : Unbounded_String) return Integer is
        Mode_Num : Integer := 999;
    begin
        Ada.Text_IO.Put_Line     ("DEBUG: Decode_Mode_US got: " & To_String(Mode));
        for Ix in Mode_Arr_T'First .. Mode_Arr_T'Last loop
            Ada.Text_IO.Put_Line ("DEBUG:     Comparing with: " &  To_String(Mode_Arr(Ix)));
            if Mode_Arr(Ix) = Mode then
                Mode_Num := Ix;
                exit;
            end if;
        end loop;
        if Mode_Num = 999 then
            raise Invalid_Mode_String with To_String (Mode);
        end if;
        return Mode_Num;
    end Decode_Mode_US;

    function Fan_Sweep_Str_To_Int (Sweep : String) return Integer is
        Sweep_Num : Integer := 999;
    begin
        for Ix in Fan_Sweep_Arr_T'First .. Fan_Sweep_Arr_T'Last loop
            if Fan_Sweep_Arr(Ix) = +Sweep then
                Sweep_Num := Ix;
                exit;
            end if;
        end loop;
        if Sweep_Num = 999 then
            raise Invalid_Fan_Sweep_String with Sweep;
        end if;
        return Sweep_Num;
    end Fan_Sweep_Str_To_Int;


    function Fan_Rate_C_To_US (Code : Character) return Unbounded_String is
    begin
        case Code is
            when 'A' => return +"AUTO";
            when 'B' => return +"SILENT";
            when '3' => return +"LEVEL_1";
            when '4' => return +"LEVEL_2";
            when '5' => return +"LEVEL_3";
            when '6' => return +"LEVEL_4";
            when '7' => return +"LEVEL_5";
            when others =>
                raise Invalid_Fan_Rate_Character with "" & Code;
        end case;
    end Fan_Rate_C_To_US;

    function Fan_Rate_Str_to_C (Rate : String) return Character is
    begin
        if    Rate = "AUTO"    then return 'A';
        elsif Rate = "SILENT"  then return 'B';
        elsif Rate = "LEVEL_1" then return '3';
        elsif Rate = "LEVEL_2" then return '4';
        elsif Rate = "LEVEL_3" then return '5';
        elsif Rate = "LEVEL_4" then return '6';
        elsif Rate = "LEVEL_5" then return '7';
        else
            raise Invalid_Fan_Rate_String with Rate;
        end if;
    end Fan_Rate_Str_to_C;

    function Control_Info_To_Cmd (CI : Control_Info_T) return String is
        -- result must look like this C sprintf description: "?pow=%s&mode=%s&stemp=%s&f_rate=%s&f_dir=%s&shum=%s"
        Cmd : Unbounded_String;
    begin
        Cmd := +"?pow=";
        if CI.Power then 
            Append (Cmd, "1"); 
        else 
            Append (Cmd, '0'); 
        end if;
        Cmd := Cmd & "&mode="   & Trim(CI.Mode'Image, Both);
        Cmd := Cmd & "&stemp="  & Trim(CI.Set_Temp'Image, Both);
        Cmd := Cmd & "&shum="   & Trim(CI.Set_Humidity'Image, Both);
        Cmd := Cmd & "&f_rate=" & CI.Fan_Rate;
        Cmd := Cmd & "&f_dir="  & Trim(CI.Fan_Sweep'Image, Both);
        return To_String (Cmd);
    end Control_Info_To_Cmd;

end Infos;