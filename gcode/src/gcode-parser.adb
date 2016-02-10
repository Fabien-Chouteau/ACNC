with Ada.Text_IO; use Ada.Text_IO;
with Gcode.Lexer; use Gcode.Lexer;
with Gcode.Shunting_Yard; use Gcode.Shunting_Yard;
with Gcode.Parameters; use Gcode.Parameters;
with Gcode.Error; use Gcode.Error;
with Ada.Exceptions; use Ada.Exceptions;

package body Gcode.Parser is

   -----------
   -- Parse --
   -----------

   function Parse (Line : string; Ctx : in out GContext'Class)
                   return Boolean
   is
      Tokens : Token_List;
      Cur : Token_Range := Token_Range'First;

      ---------------
      -- Increment --
      ---------------

      procedure Increment is
      begin
         Cur := Cur + 1;
      end Increment;

      -------------
      -- Current --
      -------------

      function Current return Token is
      begin
         return Get (Tokens, Cur);
      end Current;

      ----------------
      -- Parse_Word --
      ----------------

      procedure Parse_Word is
         Value  : Float_Value;
         Letter : Word_Letter;
      begin
         if Current.Ttype /= Gcode.Lexer.Word then
            Raise_Error (Ctx,
                         "Word letter expected at " & Current.Tstart'Img);
            return;
         end if;

         if Line (Current.Tstart) not in Word_Letter then
            Raise_Error (Ctx,
                        "Unknown word letter: " &  Line (Current.Tstart));
            return;
         end if;

         Letter := Line (Current.Tstart);
         Increment;
         Value := Evaluate_Expression (Line   => Line,
                                       Ctx    => Ctx,
                                       Tokens => Tokens,
                                       Cur    => Cur);
         --  Put_Line ("Parse_Word : " & Letter'Img & " := " & Value'Img);
         Ctx.B (Letter).Is_Set := True;
         Ctx.B (Letter).Value := Value;
      end Parse_Word;

      -----------------------
      -- Parse_Line_Number --
      -----------------------

      procedure Parse_Line_Number is
      begin
         if Current.Ttype = Literal then
            Increment;
         else
            Raise_Error (Ctx,
                         "Literral expected at :" & Current.Tstart'Img);
         end if;
      end Parse_Line_Number;

      -----------------------------
      -- Parse_Param_Declaration --
      -----------------------------

      procedure Parse_Param_Declaration is
         Param_Token : Token;
         Value       : Float_Value;
      begin
         if Current.Ttype /= Param then
            Raise_Error (Ctx,
                        "'#' expected at " & Current.Tstart'Img &
                           " " & Current.Ttype'Img & " found");
            return;
         end if;
         Increment;
         case Current.Ttype is
            when Param_Name | Literal  =>
               Param_Token := Current;
            when others =>
               Raise_Error (Ctx,
                            "Parameter Id expected at " & Current.Tstart'Img &
                              " " & Current.Ttype'Img & " found");
               return;
         end case;

         Increment;

         if Current.Ttype /= Assign then
            Raise_Error (Ctx,
                         "Assignment expected at " & Current.Tstart'Img);
            return;
         end if;

         Increment;

         Value := Evaluate_Expression (Line, Ctx, Tokens, Cur);

         if not Error_Raised (Ctx) then
            case Param_Token.Ttype is
               when Param_Name => null;
                  Define (Ctx.Params,
                          Line (Param_Token.Tstart + 1 .. Param_Token.Tend - 1),
                          Value);
               when Literal =>
                  Define (Ctx.Params, Parameter_Id (Param_Token.Value), Value);
               when others => null;
            end case;
         end if;
      end Parse_Param_Declaration;

   begin
      Clear_Error (Ctx);
      for W of Ctx.B loop
         W.Is_Set := False;
      end loop;

      Tokenize (Line, Ctx, Tokens);

      while not Error_Raised (Ctx) and then Current.Ttype /= Unknown_Token loop
         case Current.Ttype is
         when Line_Number => Parse_Line_Number;
         when Param => Parse_Param_Declaration;
         when Gcode.Lexer.Word => Parse_Word;
         when Comment => Increment;
         when others =>
            Raise_Error (Ctx,
                         "Unexpected token " & Current.Ttype'Img &" at " &
                           Current.Tstart'Img);
         end case;
      end loop;
      return not Error_Raised (Ctx);
   exception
      when E : Gcode_Exception =>
         Put_Line (Exception_Message (E));
         return False;
   end Parse;
end Gcode.Parser;
