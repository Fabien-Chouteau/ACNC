with Settings;
with Ada.Text_IO; use Ada.Text_IO;
with Gcode.Error; use Gcode.Error;

package body Gcode.Context is

   -------------------
   -- Step_To_Milli --
   -------------------

   function Step_To_Milli (Ctx : in out GContext; S : Step_Position)
                           return Float_Position
   is
      pragma Unreferenced (Ctx);
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

   function Milli_To_Step (Ctx : in out GContext; S : Float_Position)
                           return Step_Position
   is
      pragma Unreferenced (Ctx);
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

   -----------------
   -- Raise_Error --
   -----------------

   procedure Raise_Error (Ctx : in out GContext; Msg : String) is
   begin
      Ctx.Error_Flag := True;
      Ctx.Put_Line (Msg);
      raise Gcode_Exception with Msg;
   end Raise_Error;

   -----------------
   -- Clear_Error --
   -----------------

   procedure Clear_Error (Ctx : in out GContext) is
   begin
      Ctx.Error_Flag := False;
   end Clear_Error;

   ------------------
   -- Error_Raised --
   ------------------

   function Error_Raised (Ctx : in out GContext) return Boolean is
   begin
      return Ctx.Error_Flag;
   end Error_Raised;

   ----------
   -- Home --
   ----------

   function Home (Ctx : in out GContext; Axis : Axis_Name)
                  return Boolean is
      pragma Unreferenced (Axis, Ctx);
   begin
      return False;
   end Home;

   ------------------
   -- Report_Error --
   ------------------

   procedure Report_Error (Ctx : in out GContext;
                           Line, Msg : String;
                           EStart, EEnd : Natural) is
   begin
      Ctx.Error_Flag := True;
      Ctx.Put_Line (Line);
      if EStart /= 0 and then EEnd /= 0 then
         for Index in Line'First .. EStart - 1 loop
            Put (GContext'Class (Ctx), ' ');
         end loop;
         Put (GContext'Class (Ctx), '^');
         if EStart /= EEnd then
            for Index in EStart + 1 .. EEnd - 1 loop
               Put (GContext'Class (Ctx), '-');
            end loop;
            Put (GContext'Class (Ctx), '^');
         end if;
         Ctx.New_Line;
      end if;
      Ctx.Raise_Error ("Error: " & Msg);
   end Report_Error;

   ------------------
   -- Report_Error --
   ------------------

   procedure Report_Error (Ctx : in out GContext;
                           Line, Msg : String;
                           EStart : Natural) is
   begin
      Report_Error (Ctx, Line, Msg, EStart, EStart);
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

   procedure Put (Ctx : in out GContext; Str : String) is
   begin
      for C of Str loop
         Put (GContext'Class (Ctx), C);
      end loop;
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (Ctx : in out GContext; Str : String) is
   begin
      Ctx.Put (Str);
      Ctx.New_Line;
   end Put_Line;

   --------------
   -- New_Line --
   --------------

   procedure New_Line (Ctx : in out GContext) is
   begin
      Ctx.Put (ASCII.CR & ASCII.LF);
   end New_Line;

end Gcode.Context;
