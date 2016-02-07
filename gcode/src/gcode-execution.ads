with Gcode.Context; use Gcode.Context;

package Gcode.Execution is
   function Execute (Line : String; Ctx : in out GContext'Class) return Boolean;
end Gcode.Execution;
