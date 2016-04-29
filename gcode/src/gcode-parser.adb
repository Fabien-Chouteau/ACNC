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

   function Parse (Line : String; Ctx : in out GContext'Class)
                   return Boolean
   is
      Tokens : Token_List;
      Cur : Token_Range := Token_Range'First;

      procedure Increment;
      function Current return Token;
      procedure Parse_Word;
      procedure Parse_Line_Number;
      procedure Parse_Param_Declaration;

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
            Ctx.Report_Error (Line   => Line,
                              Msg    => "Word letter expected",
                              EStart => Current.Tstart);
            return;
         end if;

         if Line (Current.Tstart) not in Word_Letter then
            Ctx.Report_Error (Line   => Line,
                              Msg    => "Unknown word letter",
                              EStart => Current.Tstart);
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
            Ctx.Report_Error (Line   => Line,
                              Msg    => "Literral expected",
                              EStart => Current.Tstart);
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
            Ctx.Report_Error (Line   => Line,
                              Msg    => "'#' expected",
                              EStart => Current.Tstart);
            return;
         end if;
         Increment;
         case Current.Ttype is
            when Param_Name | Literal  =>
               Param_Token := Current;
            when others =>
               Ctx.Report_Error (Line   => Line,
                                 Msg    => "Parameter Id expected, " &
                                   Current.Ttype'Img & " found",
                                 EStart => Current.Tstart);
               return;
         end case;

         Increment;

         if Current.Ttype /= Assign then
               Ctx.Report_Error (Line   => Line,
                                 Msg    => "Assignment expected, " &
                                   Current.Ttype'Img & " found",
                                 EStart => Current.Tstart);
            return;
         end if;

         Increment;

         Value := Evaluate_Expression (Line, Ctx, Tokens, Cur);

         if not Error_Raised (Ctx) then
            case Param_Token.Ttype is
               when Param_Name => null;
                  Define
                    (Ctx.Params,
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

      while not Error_Raised (Ctx) and then Current.Ttype /= End_Of_Line loop
         case Current.Ttype is
         when Line_Number => Parse_Line_Number;
         when Param => Parse_Param_Declaration;
         when Gcode.Lexer.Word => Parse_Word;
         when Comment => Increment;
         when others =>
            Ctx.Report_Error
              (Line   => Line,
               Msg    => "Unexpected token " & Current.Ttype'Img,
               EStart => Current.Tstart);
         end case;
      end loop;
      return not Error_Raised (Ctx);
   exception
      when E : Gcode_Exception =>
         Put_Line (Exception_Message (E));
         return False;
   end Parse;
end Gcode.Parser;
