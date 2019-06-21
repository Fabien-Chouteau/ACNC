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

package Gcode.Parameters is

   Max_Parameter : constant := 50;
   type Param_Type is (Undefined, Named, Numbered);
   type Parameters_Set is private;
   subtype Parameter_Id is Natural;

   type String_Access is access all String;

   type Parameter is record
      Ptype : Param_Type := Undefined;
      Id    : Parameter_Id;
      Name  : String_Access;
      Value : Float_Value := 0.0;
   end record;

   Undefined_Param : constant Parameter := (Undefined, 0, null, 0.0);

   procedure Define (Ctx   : in out Parameters_Set;
                     Id    : Parameter_Id;
                     Value : Float_Value);
   procedure Define (Ctx   : in out Parameters_Set;
                     Name  : String;
                     Value : Float_Value);
   function Defined (Ctx : Parameters_Set;
                     Id  : Parameter_Id) return Boolean;
   function Defined (Ctx  : Parameters_Set;
                     Name : String) return Boolean;
   function Get_Value (Ctx : Parameters_Set;
                       Id  : Parameter_Id) return Float_Value;
   function Get_Value (Ctx  : Parameters_Set;
                       Name : String) return Float_Value;

   procedure Clear (Ctx : in out Parameters_Set);

private

   subtype Parameter_Range is Natural range 1 .. Max_Parameter;
   type Parameter_Array is array (Parameter_Range) of Parameter;

   type Parameters_Set is record
      Params : Parameter_Array := (others => Undefined_Param);
      Last   : Natural := Parameter_Range'First;
   end record;
end Gcode.Parameters;
