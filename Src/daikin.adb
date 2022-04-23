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

package body Daikin is

    protected body State is

        procedure Init (Conf : in Config.Daikin_T; Inv_Conf : in Config.Inverters_T; Verbose : in Boolean) is
            Inverter_Status : Inverter_Status_T;
            I               : Config.Inverter_T;
        begin
            Daikin_Conf := Conf;
            for Ix in 1 .. Config.Inverter_Count loop
                I := Inv_Conf(Ix);
                if Inverter_Maps.Contains (Inverters_By_Name, I.Friendly_Name) then
                    raise Config.Duplicate_Configuration with "configured multiple times: " & To_String (I.Friendly_Name);
                end if;
                Inverter_Status.Use_IP_Addr := I.Use_IP_Addr;
                Inverter_Status.MAC_Addr    := I.MAC_Addr;
                Inverter_Status.IP_Addr     := I.IP_Addr;
                Inverter_Status.Configured  := True;
                Inverter_Status.Detected    := False;
                Inverter_Status.Online      := False;
                Inverter_Maps.Insert (Container => Inverters_By_Name, Key => I.Friendly_Name, New_Item => Inverter_Status);
            end loop;
        end Init;

        
    end State;

end Daikin;