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

with Settings;

package body Gcode is

   ---------------
   -- To_Letter --
   ---------------

   function To_Letter (Axis : Axis_Name) return Character is
   begin
      case Axis is
         when X_Axis => return 'X';
         when Y_Axis => return 'Y';
         when Z_Axis => return 'Z';
      end case;
   end To_Letter;

   -----------------
   -- Reverse_Dir --
   -----------------

   procedure Reverse_Dir (Dir : in out Direction) is
   begin
      Dir := (if Dir = Forward then Backward else Forward);
   end Reverse_Dir;

   -------------------
   -- Step_To_Milli --
   -------------------

   function Step_To_Milli (S : Step_Position) return Float_Position is
      Ret : Float_Position;
   begin
      for Axis in Axis_Name loop
         Ret (Axis) :=
           Float_Value (S (Axis)) / Settings.Step_Per_Millimeter (Axis);
      end loop;
      return Ret;
   end Step_To_Milli;

   -------------------
   -- Milli_To_Step --
   -------------------

   function Milli_To_Step (S : Float_Position) return Step_Position is
      Ret : Step_Position;
   begin
      for Axis in Axis_Name loop
         Ret (Axis) :=
           Steps (S (Axis) * Settings.Step_Per_Millimeter (Axis));
      end loop;
      return Ret;
   end Milli_To_Step;

   -------------------
   -- Inch_To_Milli --
   -------------------

   function Inch_To_Milli (S : Float_Position) return Float_Position is
      Ret : Float_Position;
   begin
      for Axis in Axis_Name loop
         Ret (Axis) := Inch_To_Milli (S (Axis));
      end loop;
      return Ret;
   end Inch_To_Milli;

   -------------------
   -- Inch_To_Milli --
   -------------------

   function Inch_To_Milli (S : Float_Value) return Float_Value is
   begin
      return S * 25.4;
   end Inch_To_Milli;
   --------------
   -- Distance --
   --------------

   function Distance (A, B : Float_Position) return Float_Value is
      Tmp : Float_Value := 0.0;
      function Sqrt (X : Float_Value) return Float_Value;

      ----------
      -- Sqrt --
      ----------

      function Sqrt (X : Float_Value) return Float_Value is
         U     : Float_Value := X;
         New_U : Float_Value;
      begin
         if X < 0.0 then
            raise Program_Error;
         end if;
         if X = 0.0 then
            return 0.0;
         end if;
         loop
            New_U := (U + (X / U)) / 2.0;
            exit when New_U >= U;
            U := New_U;
         end loop;
         return U;
      end Sqrt;
   begin
      for Axis in Axis_Name loop
         Tmp := Tmp + (A (Axis) - B (Axis))**2;
      end loop;
      return Sqrt (Tmp);
   exception
      when others =>
         return 0.0;
   end Distance;

   -----------
   -- Image --
   -----------

   function Image (Val : Float_Value) return String is
      Floor : constant Float_Value := Float_Value'Floor (Val);
      Int_Part : constant Integer := Integer (Floor);
      Frac_Part : constant Integer :=
        Integer (Float_Value'Floor ((Val - Floor) * 10000.0));
   begin
      return Int_Part'Img & "." & Frac_Part'Img;
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Pos : Float_Position) return String is
   begin
      return ("X:" & Image (Pos (X_Axis)) & " Y:" & Image (Pos (Y_Axis))
              & " Z:" & Image (Pos (Z_Axis)));
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Pos : Step_Position) return String is
   begin
      return ("X:" & Pos (X_Axis)'Img & " Y:" & Pos (Y_Axis)'Img & " Z:" &
                Pos (Z_Axis)'Img);
   end Image;

end Gcode;
