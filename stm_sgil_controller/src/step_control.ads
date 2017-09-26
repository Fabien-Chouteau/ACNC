-------------------------------------------------------------------------------
--                                                                           --
--                                   ACNC                                    --
--                                                                           --
--         Copyright (C) 2016 Fabien Chouteau (chouteau@adacore.com)         --
--                                                                           --
--                                                                           --
--    ACNC is free software: you can redistribute it and/or modify it        --
--    under the terms of the GNU General Public License as published by      --
--    the Free Software Foundation, either version 3 of the License, or      --
--    (at your option) any later version.                                    --
--                                                                           --
--    ACNC is distributed in the hope that it will be useful, but WITHOUT    --
--    ANY WARRANTY; without even the implied warranty of MERCHANTABILITY     --
--    or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public        --
--    License for more details.                                              --
--                                                                           --
--    You should have received a copy of the GNU General Public License      --
--    along with ACNC. If not, see <http://www.gnu.org/licenses/>.           --
--                                                                           --
-------------------------------------------------------------------------------

with STM32.Device; use STM32.Device;
with STM32.GPIO; use STM32.GPIO;
with Gcode; use Gcode;

package Step_Control is
   procedure Initalize;
private

   type GPIO_Point_Per_Axis is array (Axis_Name) of GPIO_Point;

   M1_Step_GPIO : GPIO_Point renames PD8;
   M2_Step_GPIO : GPIO_Point renames PD10;
   M3_Step_GPIO : GPIO_Point renames PB10;

   M1_Dir_GPIO : GPIO_Point renames PD15;
   M2_Dir_GPIO : GPIO_Point renames PD12;
   M3_Dir_GPIO : GPIO_Point renames PB12;

   M1_Not_Enable_GPIO : GPIO_Point renames PD14;
   M2_Not_Enable_GPIO : GPIO_Point renames PB14;
   M3_Not_Enable_GPIO : GPIO_Point renames PE14;

   Step_GPIO : GPIO_Point_Per_Axis :=
     (X_Axis => M2_Step_GPIO,
      Y_Axis => M1_Step_GPIO,
      Z_Axis => M3_Step_GPIO);

   Dir_GPIO : GPIO_Point_Per_Axis :=
     (X_Axis => M2_Dir_GPIO,
      Y_Axis => M1_Dir_GPIO,
      Z_Axis => M3_Dir_GPIO);

   Not_Enable_GPIO : GPIO_Point_Per_Axis :=
     (X_Axis => M2_Not_Enable_GPIO,
      Y_Axis => M1_Not_Enable_GPIO,
      Z_Axis => M3_Not_Enable_GPIO);

   Home_GPIO : GPIO_Point_Per_Axis :=
     (X_Axis => PC11,
      Y_Axis => PD2,
      Z_Axis => PD0);

   Home_Switch_Polarity : constant array (Axis_Name) of Boolean :=
     (X_Axis => True,
      Y_Axis => True,
      Z_Axis => False);


   Analysis_Point : GPIO_Point renames PD13;
   --  This GPIO can be used to mesure the time spent en step event
end Step_Control;
