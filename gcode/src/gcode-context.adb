with Ada.Text_IO; use Ada.Text_IO;
with Gcode.Error; use Gcode.Error;

package body Gcode.Context is

   -------------------
   -- Step_To_Milli --
   -------------------

   function Step_To_Milli (Ctx : in out GContext; S : Step_Position)
                           return Float_Position
   is
   begin
      return (Long_Float (S.X) / Ctx.Step_Per_Millimeter.X,
              Long_Float (S.Y) / Ctx.Step_Per_Millimeter.Y,
              Long_Float (S.Z) / Ctx.Step_Per_Millimeter.Z);
   end;

   -------------------
   -- Milli_To_Step --
   -------------------

   function Milli_To_Step (Ctx : in out GContext; S : Float_Position)
                           return Step_Position
   is
   begin
      return (Steps (S.X * Ctx.Step_Per_Millimeter.X),
              Steps (S.Y * Ctx.Step_Per_Millimeter.Y),
              Steps (S.Z * Ctx.Step_Per_Millimeter.Z));
   end;

   -------------------
   -- Inch_To_Milli --
   -------------------

   function Inch_To_Milli (S : Float_Position) return Float_Position is
   begin
      return (S.X * 25.4, S.Y * 25.4, S.Z * 25.4);
   end;

   -------------------
   -- Inch_To_Milli --
   -------------------

   function Inch_To_Milli (S : Long_Float) return Long_Float is
   begin
      return S * 25.4;
   end;

   -----------------
   -- Raise_Error --
   -----------------

   procedure Raise_Error (Ctx : in out GContext; Msg : String) is
   begin
      Ctx.Error_Flag := True;
      raise Gcode_Exception with Msg;
   end;

   -----------------
   -- Clear_Error --
   -----------------

   procedure Clear_Error (Ctx : in out GContext) is
   begin
      Ctx.Error_Flag := False;
   end;

   ------------------
   -- Error_Raised --
   ------------------

   function Error_Raised (Ctx : in out GContext) return Boolean is
   begin
      return Ctx.Error_Flag;
   end;

   ----------
   -- Home --
   ----------

   function Home (Ctx : in out GContext; Axis : Axis_Name)
                  return Boolean is
      pragma Unreferenced (Axis, Ctx);
   begin
      return False;
   end;

   ----------
   -- Step --
   ----------

   procedure Step (Ctx : in out GContext; Axis : Axis_Name; Dir : Direction) is
      S : constant Steps := (if Dir = Forward then 1 else -1);
   begin
      case Axis is
         when X_Axis =>
            --  Put_Line ("Step X " & Dir'Img);
            Ctx.Real_Position.X := Ctx.Real_Position.X + S;
         when Y_Axis =>
            --  Put_Line ("Step Y " & Dir'Img);
            Ctx.Real_Position.Y := Ctx.Real_Position.Y + S;
         when Z_Axis =>
            --  Put_Line ("Step Z " & Dir'Img);
            Ctx.Real_Position.Z := Ctx.Real_Position.Z + S;
      end case;
   end Step;

   ------------------
   -- Report_Error --
   ------------------

   procedure Report_Error (Ctx : in out GContext;
                           Line, Msg : string;
                           EStart, EEnd : Natural) is
   begin
      Ctx.Error_Flag := True;
      Ctx.Put_Line (Line);
      if EStart /= 0 and then EEnd /= 0 then
         for Index in Line'First .. EStart - 1 loop
            Ctx.Put (' ');
         end loop;
         Ctx.Put ('^');
         if EStart /= EEnd then
            for Index in EStart + 1 .. EEnd - 1 loop
               Ctx.Put ('-');
            end loop;
            Ctx.Put ('^');
         end if;
         Ctx.New_Line;
      end if;
      Ctx.Put_Line ("Error: " & Msg);
      --  Ctx.Log (Error, Msg);
   end Report_Error;

   ---------
   -- Log --
   ---------

   procedure Log (Ctx : in out GContext; Lvl : Log_Level; Str : String) is
      pragma Unreferenced (Ctx);
   begin
      Put ((case Lvl is
              when Info    => "Info: ",
              when Warning => "Warning: ",
              when Error   => "Error: ",
              when Board   => "Board: ") & Str);
      New_Line;
   end Log;

   ---------
   -- Put --
   ---------

   procedure Put (Ctx : in out GContext; C : Character) is
      pragma Unreferenced (Ctx);
   begin
      Put (C);
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (Ctx : in out GContext; Str : String) is
      pragma Unreferenced (Ctx);
   begin
      Put (Str);
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (Ctx : in out GContext; Str : String) is
      pragma Unreferenced (Ctx);
   begin
      Put_Line (Str);
   end Put_Line;

   --------------
   -- New_Line --
   --------------

   procedure New_Line (Ctx : in out GContext) is
      pragma Unreferenced (Ctx);
   begin
      New_Line;
   end New_Line;

end Gcode.Context;
