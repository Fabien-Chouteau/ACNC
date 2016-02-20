with Gcode.Context; use Gcode.Context;

package Gcode_Controller is

   procedure Initalize;
   procedure Execute (Str : String);

   type CNC_Context is new GContext with private;

   overriding
   procedure Put (Ctx : in out CNC_Context; C : Character);

private
   subtype Buffer_Range is Positive range 1 .. 256;

   type CNC_Context is new GContext with record
      Output_Buffer : String (Buffer_Range);
      Output_Index : Buffer_Range := Buffer_Range'First;
   end record;

   Ctx : CNC_Context;
end Gcode_Controller;
