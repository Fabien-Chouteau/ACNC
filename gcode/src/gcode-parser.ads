with Gcode.Context; use Gcode.Context;

package Gcode.Parser is
   function Parse (Line : String; Ctx : in out GContext'Class) return Boolean;
end Gcode.Parser;
