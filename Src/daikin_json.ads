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

-- with Ada.Containers.Ordered_Maps;

with Daikin_Data;     use Daikin_Data;

package Daikin_JSON is

    function Bool_To_JSON (ItIs : Boolean) return String;
    function Fan_Rate_To_String (FR : Character) return String;

    function BI_To_JSON (BI : Basic_Info_T)   return String;
    function CI_To_JSON (CI : Control_Info_T) return String;
    function SI_To_JSON (SI : Sensor_Info_T)  return String;

    Unknown_Fan_Rate,
    Unknown_Inverter_Name : exception;

end Daikin_JSON;