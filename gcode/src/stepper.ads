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

with Gcode; use Gcode;

package Stepper is
   function Execute_Step_Event return Boolean;

   type Set_Step_Pin_Proc is not null access
     procedure (Axis : Axis_Name);

   type Clear_Step_Pin_Proc is not null access
     procedure (Axis : Axis_Name);

   type Set_Direction_Pin_Proc is not null access
     procedure (Axis : Axis_Name; Dir : Direction);

   type Set_Stepper_Frequency_Proc is not null access
     procedure (Freq_Hz : Frequency_Value);

   type Home_Test_Proc is not null access
     function (Axis : Axis_Name) return Boolean;

   type Motor_Enable_Proc is not null access
     procedure (Axis : Axis_Name; Enable : Boolean);


   procedure Set_Stepper_Callbacks
     (Set_Step              : Set_Step_Pin_Proc;
      Clear_Step            : Clear_Step_Pin_Proc;
      Set_Direcetion        : Set_Direction_Pin_Proc;
      Set_Stepper_Frequency : Set_Stepper_Frequency_Proc;
      Home_Test             : Home_Test_Proc;
      Motor_Enable          : Motor_Enable_Proc);

end Stepper;
