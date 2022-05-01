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
with Ada.Calendar.Formatting;
with Ada.Text_IO;           use Ada.Text_IO;
with AWS.Client, AWS.Response, AWS.Messages;

with GNAT.String_Split;
with GNATCOLL.JSON;

with Daikin_JSON;  use Daikin_JSON;

package body Daikin is
  
    procedure Warning (Message : String) is
    begin
        Put_Line (Ada.Calendar.Formatting.Image (Ada.Calendar.Clock) & " WARNING: " & Message);
    end Warning;

    procedure Error (Message : String) is
    begin
        Put_Line (Ada.Calendar.Formatting.Image (Ada.Calendar.Clock) & " ERROR: " & Message);
    end Error;

    procedure Fetch_Basic_Info (IP_Addr : String; BI : out Basic_Info_T; OK : out Boolean) is
        Resp   : AWS.Response.Data;
        Status : AWS.Messages.Status_Code;
        Query  : constant String := "http://" & IP_addr & Get_Basic_Info;
    begin
        OK := True;
        Resp   := AWS.Client.Get (URL => Query, Timeouts => AWS.Client.Timeouts(Each => 1.0));
        Status := AWS.Response.Status_Code (Resp);
        if Status not in AWS.Messages.Success then
            Warning ("Basic Information request failed with error: " & Status'Image & 
                      " from HTTP query: " & Query);
            OK := False;
        else
            declare 
                Resp_Body : constant String := AWS.Response.Message_Body (Resp);
            begin
                BI := Parse_Basic_Info (Resp_Body);
            end;
        end if;
    end Fetch_Basic_Info;

    procedure Fetch_And_Publish_Basic_Info (IP_Addr : String) is
        BI : Basic_Info_T;
        OK : Boolean;
    begin
        Fetch_Basic_Info (IP_Addr, BI, OK);
        if OK then
            declare
                Subtopic : constant String := "/" & State.IP_Addr_To_Name(IP_Addr) & "/basic";
            begin
                MQTT_Pub (Subtopic, BI_To_JSON (BI));
            end;
        end if;
    end Fetch_And_Publish_Basic_Info;

    procedure Fetch_Control_Info (IP_Addr : String; CI : out Control_Info_T; OK : out Boolean) is
        Resp   : AWS.Response.Data;
        Status : AWS.Messages.Status_Code;
        Query  : constant String := "http://" & IP_addr & Get_Control_Info;
    begin
        OK := True;
        Resp   := AWS.Client.Get (URL => Query, Timeouts => AWS.Client.Timeouts(Each => 1.0));
        Status := AWS.Response.Status_Code (Resp);
        if Status not in AWS.Messages.Success then
            Warning ("Control Information request failed with error: " & Status'Image & 
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

    procedure Fetch_And_Publish_Control_Info (IP_Addr : String) is
        CI : Control_Info_T;
        OK : Boolean;
    begin
        Fetch_Control_Info (IP_Addr, CI, OK);
        if OK then
            declare
                Subtopic : constant String := "/" & State.IP_Addr_To_Name(IP_Addr) & "/controls";
            begin
                MQTT_Pub (Subtopic, CI_To_JSON (CI));
            end;
        end if;
    end Fetch_And_Publish_Control_Info;

    procedure Send_Control_Info (IP_Addr : String; JSON_CI : String) is
        use GNATCOLL.JSON;
        User_Fields : JSON_Value :=  Create;
        New_CI      : Control_Info_T;
        OK          : Boolean;
        Resp        : AWS.Response.Data;
        Status      : AWS.Messages.Status_Code;
    begin
        if State.Is_Verbose then
            Put_Line ("INFO: Send_Control_Info got: " & JSON_CI);
        end if;
        User_Fields := Read (JSON_CI);
        if User_Fields = JSON_Null then
            Warning ("'set' command received with no parameters - ignoring.");
            return;
        end if;
        -- do we have the complete minimal set of controls required from the user?
        if User_Fields.Has_Field ("power") and then
           User_Fields.Has_Field ("mode")  and then
           User_Fields.Has_Field ("set_temp")  and then
           User_Fields.Has_Field ("set_humidity")  and then
           User_Fields.Has_Field ("fan_rate")  and then
           User_Fields.Has_Field ("fan_sweep") then
            -- no point fetching the old data
            New_CI.Power := User_Fields.Get ("power");
            New_CI.Mode  := Decode_Mode_US (+User_Fields.Get ("mode"));
            New_CI.Set_Temp := User_Fields.Get ("set_temp");
            New_CI.Set_Humidity := User_Fields.Get ("set_humidity");
            New_CI.Fan_Rate := Fan_Rate_Str_to_C (User_Fields.Get ("fan_rate"));
            New_CI.Fan_Sweep := Fan_Sweep_Str_To_Int (User_Fields.Get ("fan_sweep"));
        else
            Fetch_Control_Info (IP_Addr, New_CI, OK);
            if not OK then
                Warning ("Unable to handle set/controls request. Could not fetch Control Info for " & IP_Addr);
                return;
            end if;
            if User_Fields.Has_Field ("power") then
                New_CI.Power := User_Fields.Get ("power");
            end if;
            if User_Fields.Has_Field ("set_temp") then
                New_CI.Set_Temp := User_Fields.Get ("set_temp");
            end if;
            if User_Fields.Has_Field ("mode") then
                New_CI.Mode  := Decode_Mode_US (+User_Fields.Get ("mode"));
                case New_CI.Mode is
                    when 0 | 1 | 4 | 7 =>
                        if New_CI.Set_Temp = 0 then
                            -- the user has set to heat, but not yet set the temperature - default to 20C
                            New_CI.Set_Temp := 20;
                        end if;
                    when others => null;    
                end case;
            end if;
            if User_Fields.Has_Field ("set_humidity") then
                New_CI.Set_Humidity := User_Fields.Get ("set_humidity");
            end if;
            if User_Fields.Has_Field ("fan_rate") then
                New_CI.Fan_Rate := Fan_Rate_Str_to_C (User_Fields.Get ("fan_rate"));
            end if;
            if User_Fields.Has_Field ("fan_sweep") then
                New_CI.Fan_Sweep := Fan_Sweep_Str_To_Int (User_Fields.Get ("fan_sweep"));
            end if;
        end if;
        delay 0.15;
        if State.Is_Verbose then
            Put_Line ("INFO: Will send set command: " & "http://" & IP_Addr & Set_Control_Info & Control_Info_To_Cmd (New_CI));
        end if;
        Resp   := AWS.Client.Get (URL => "http://" & IP_Addr & Set_Control_Info & Control_Info_To_Cmd (New_CI), Timeouts => AWS.Client.Timeouts(Each => 2.0));
        Status := AWS.Response.Status_Code (Resp);
        if Status not in AWS.Messages.Success then
            Warning ("set/controls request failed with error: " & Status'Image & 
                      " from HTTP query: " & "http://" & IP_Addr & Set_Control_Info & Control_Info_To_Cmd (New_CI));
        end if;
        delay 0.15;
        Fetch_And_Publish_Control_Info (IP_Addr);
    exception
        when others =>
            Error ("Exception caught handling set/controls - ignoring this request.");
    end Send_Control_Info;

    procedure Fetch_Sensor_Info (IP_Addr : String; SI : out Sensor_Info_T; OK : out Boolean) is
        Resp   : AWS.Response.Data;
        Status : AWS.Messages.Status_Code;
        Query  : constant String := "http://" & IP_addr & Get_Sensor_Info;
    begin
        OK := True;
        Resp   := AWS.Client.Get (URL => Query, Timeouts => AWS.Client.Timeouts(Each => 1.0));
        Status := AWS.Response.Status_Code (Resp);
        if Status not in AWS.Messages.Success then
            Warning ("Sensor Information request failed with error: " & Status'Image &  
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

    procedure Fetch_And_Publish_Sensor_Info (IP_Addr : String) is
        SI : Sensor_Info_T;
        OK : Boolean;
    begin
        Fetch_Sensor_Info (IP_Addr, SI, OK);
        if OK then
            declare
                Subtopic : constant String := "/" & State.IP_Addr_To_Name(IP_Addr) & "/sensors";
            begin
                MQTT_Pub (Subtopic, SI_To_JSON (SI));
            end;
        end if;
    end Fetch_And_Publish_Sensor_Info;

    task body Monitor_Units is
        Period     : Duration;
        Monitoring : Boolean;
        IP_Addr    : Unbounded_String;
        SI         : Sensor_Info_T;
        CI         : Control_Info_T;
        OK         : Boolean;
    begin
        accept Start (Period_S : Duration) do
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
                    if State.Is_Verbose then
                        Put_Line ("DEBUG: Monitor_Units fetching status of: " & To_String (I_Name));
                    end if;
                    Inv_Stat := State.Get_Inverter_Status (I_Name);
                    IP_Addr  := Inv_Stat.IP_Addr;
                    if State.Is_Verbose then
                        Put_Line ("DEBUG: ... Monitor_Units fetching Sensor Info");
                    end if;
                    Fetch_Sensor_Info (IP_Addr => To_String(IP_Addr), SI => SI , OK => OK);
                    if OK then
                        State.Set_Sensor_Info (I_Name, SI);
                        MQTT_Pub ("/" & To_String(I_Name) & "/sensors", SI_To_JSON (SI));
                    end if;
                    delay 0.15;
                    if State.Is_Verbose then
                        Put_Line ("DEBUG: ... Monitor_Units fetching Control Info");
                    end if;
                    Fetch_Control_Info (IP_Addr => To_String(IP_Addr), CI => CI, OK => OK);
                    if OK then
                        State.Set_Control_Info (I_Name, CI);
                        MQTT_Pub ("/" & To_String(I_Name) & "/controls", CI_To_JSON (CI));
                    end if;
                    if State.Is_Verbose then
                        Put_Line ("DEBUG: ... Monitor_Units done for this unit");
                    end if;  
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

        procedure Init (Conf : Config.Daikin_T; Inv_Conf : Config.Inverters_T; Verbose : Boolean) is
            Inverter_Status : Inverter_Status_T;
            I               : Config.Inverter_T;
            IC              : Status_Maps.Cursor;
            BI              : Basic_Info_T;
            OK              : Boolean;
        begin
            State.Verbose := Verbose;
            Daikin_Conf := Conf;
            for Ix in 1 .. Config.Inverter_Count loop
                I := Inv_Conf(Ix);
                if Status_Maps.Contains (Inverter_Statuses, I.Friendly_Name) then
                    raise Config.Duplicate_Configuration with "configured multiple times: " & To_String (I.Friendly_Name);
                end if;
                Inverter_Status.IP_Addr     := I.IP_Addr;
                Inverter_Status.Detected    := False;
                Inverter_Status.Online      := False;
                Status_Maps.Insert (Container  => Inverter_Statuses, 
                                      Key      => I.Friendly_Name, 
                                      New_Item => Inverter_Status, 
                                      Position => IC, 
                                      Inserted => OK);
                if not OK then
                    Error ("Could not configure inverter: " & To_String(I.Friendly_Name));
                elsif Verbose then
                    Put_Line ("DEBUG: Configured inverter: " & To_String(I.Friendly_Name));
                end if;

                if Verbose then
                    Put_Line ("INFO: Checking inverter: " & To_String (I.Friendly_Name));
                end if;
                Fetch_Basic_Info (IP_Addr => To_String (I.IP_Addr), 
                                  BI => BI, 
                                  OK => OK);
                if not OK then
                    Warning ("Failed to detect " & To_String (I.Friendly_Name) & 
                                " - Will assume it's out there!");
                else
                    Inverter_Status.Basic_Info := BI;
                end if;
                Set_Inverter_Online (I.Friendly_Name, True);
                Inverters_IP_To_Name.Include (I.IP_Addr, I.Friendly_Name); 
            end loop;
            Put_Line ("INFO:" & Status_Maps.Length (Inverter_Statuses)'Image & " Inverters configured");
        end Init;

        function Get_Inverter_Status (F_Name : Unbounded_String) return Inverter_Status_T is
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
            if Verbose then
                Put_Line ("INFO: Get_Online_Inverters will return" & Online_Count'Image & " inverters");
            end if;
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

        procedure Set_Control_Info (F_Name : Unbounded_String; CI : Control_Info_T) is
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

        procedure Set_Control_Info (IP_Addr : String; CI : Control_Info_T) is
        begin
            if Inverter_Xrefs.Contains (Inverters_IP_To_Name, +IP_Addr) then
                Set_Control_Info (Inverters_IP_To_Name(+IP_Addr), CI);
            else
                raise Unknown_IP_Address with IP_Addr;
            end if;
        end Set_Control_Info;

        procedure Set_Sensor_Info (F_Name : Unbounded_String; SI : Sensor_Info_T) is
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

        procedure Set_Inverter_Online (F_Name : Unbounded_String; Online : Boolean) is
            Inv_Stat : Inverter_Status_T := Get_Inverter_Status (F_Name);
        begin
            Inv_Stat.Online := Online;
            Status_Maps.Replace_Element (Container => Inverter_Statuses, 
                                         Position  => Status_Maps.Find (Inverter_Statuses, F_Name),
                                         New_Item  => Inv_Stat);
        end Set_Inverter_Online;

        function Name_To_IP_Addr (F_Name : String) return String is
            (To_String (Inverter_Statuses(To_Unbounded_String(F_Name)).IP_Addr));

        function IP_Addr_To_Name (IP_Addr : String) return String is
            (To_String (Inverters_IP_To_Name(+IP_Addr)));

        function Is_Verbose return Boolean is
            (Verbose);  

    end State;

    procedure MQTT_Connect (Conf : Config.MQTT_T) is
    begin
        MQTT_Conf    := Conf;
        Mosq_Handle.Initialize (ID => MQTT_ID, Clean_Sessions => True);
        -- Mosq_Handle.Threaded_Set (True);
        Pump.Start;
        Mosq_Handle.Set_Handler (App'Unchecked_Access);
        Mosq_Handle.Connect (Host => To_String (MQTT_Conf.Broker), 
                             Port => MQTT_Conf.Port, 
                             Keepalive => Keepalive);
        -- Mosq_Handle.UnSubscribe (Topic => "/#");
        Mosq_Handle.Subscribe (Topic => To_String (MQTT_Conf.Base_Topic) & "/+/get/+");
        Mosq_Handle.Subscribe (Topic => To_String (MQTT_Conf.Base_Topic) & "/+/set/+");
        Mosq_Handle.Publish (Topic   => To_String (MQTT_Conf.Base_Topic) & "/status", 
                             Payload => "Started", 
                             Qos     => Mosquitto.Qos_1, 
                             Retain  => False);
    end MQTT_Connect;

    procedure MQTT_Pub (Subtopic, Payload : String) is
    begin
        Mosq_Handle.Publish (Topic   => To_String (MQTT_Conf.Base_Topic) & Subtopic, 
                             Payload => Payload, 
                             Qos     => Mosquitto.Qos_1, 
                             Retain  => False);
    end MQTT_Pub;

    overriding procedure On_Message(Self    : not null access This_App_T;
                                    Mosq    : Handle_Ref;
                                    Mid     : Message_Id;
                                    Topic   : String;
                                    Payload : Ada.Streams.Stream_Element_Array;
                                    QoS     : QoS_Type;
                                    Retain  : Boolean) is
        pragma Unreferenced (Self, Mosq, Mid, QoS, Retain);
        use GNAT.String_Split;
        Subtopics     : Slice_Set;
        Overlaid_Data : String (Natural (Payload'First) .. Natural (Payload'Last));
        pragma Import (C, Overlaid_Data);
        for Overlaid_Data'Address use Payload'Address;
        Verbose : constant Boolean := True; -- FIXME
    begin
        if Verbose then
            Put_Line ("DEBUG: MQTT got message in topic: " & Topic & " with payload: " & Overlaid_Data);
        end if;
        Create (S => Subtopics, From => Topic, Separators => "/");
        if Slice_Count (Subtopics) = 4 then
            declare
                F_Name : constant String := Slice (Subtopics, 2);
            begin
                if Slice (Subtopics, 3) = "get" then
                    if Slice (Subtopics, 4) = "basic" then
                        Fetch_And_Publish_Basic_Info (State.Name_To_IP_Addr(F_Name));
                    elsif Slice (Subtopics, 4) = "controls" then
                        Fetch_And_Publish_Control_Info (State.Name_To_IP_Addr(F_Name));
                    elsif Slice (Subtopics, 4) = "sensors" then
                        Fetch_And_Publish_Sensor_Info (State.Name_To_IP_Addr(F_Name)); 
                    else
                        Warning ("Unknown 'get' request with topic: " & Topic);
                    end if;
                elsif Slice (Subtopics, 3) = "set" then
                    if Slice (Subtopics, 4) = "controls" then
                        Send_Control_Info (State.Name_To_IP_Addr(F_Name), Overlaid_Data);
                    else
                        Warning ("Unknown 'set' request with topic: " & Topic);
                    end if;
                elsif Verbose then
                    Put_Line ("DEBUG: Ignoring this message");
                end if;
            end;
        elsif Verbose then
            Put_Line ("DEBUG: Ignoring this message");
        end if;
    end On_Message;

    task body Pump_T is
    begin
        accept Start do
            Put_Line ("INFO: MQTT task started");
        end Start;
        -- The Timeout value below seems to directly influence the 'idle' CPU
        -- usage of the message pump...
        Connection.Loop_Forever (Timeout => 10.0);
        Error ("MQTT loop has exited - this should not happen.");
    end Pump_T;

end Daikin;