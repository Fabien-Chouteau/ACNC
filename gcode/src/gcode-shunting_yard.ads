with Gcode.Lexer; use Gcode.Lexer;
with Gcode.Context; use Gcode.Context;

package Gcode.Shunting_Yard is
   function Evaluate_Expression (Line   : String;
                                 Ctx : in out GContext'Class;
                                 Tokens : Token_List;
                                 Cur    : in out Token_Range)
                                 return Float_Value;
end Gcode.Shunting_Yard;
