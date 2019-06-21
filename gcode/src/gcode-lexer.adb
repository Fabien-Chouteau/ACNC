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

with Gcode.Error; use Gcode.Error;

package body Gcode.Lexer is

   function Eval_Number (Line : String; Tok : Token) return Float_Value;

   ------------
   -- Append --
   ------------

   procedure Append (Tokens : in out Token_List; Tok : Token) is
   begin
      if Tokens.Last in Token_Range then
         Tokens.Tokens (Tokens.Last) := Tok;
         Tokens.Last := Tokens.Last + 1;
      else
         raise Gcode_Exception with "Max token limit reached";
      end if;
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append (Tokens : in out Token_List;
                     Ttype : Token_Type;
                     Tstart, Tend : Natural;
                     Value  : Float_Value := 0.0)
   is
   begin
      Append (Tokens, (Tstart, Tend, Ttype, Value));
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append (Tokens : in out Token_List;
                     Ttype : Token_Type;
                     Position : Natural;
                     Value  : Float_Value := 0.0)
   is
   begin
      Append (Tokens, Ttype, Position, Position, Value);
   end Append;

   ------------
   -- Insert --
   ------------

   procedure Insert (Tokens : in out Token_List; Tok : Token;
                     After : Token_Range) is
   begin
      if After >= Tokens.Last or else Tokens.Last not in Token_Range then
         raise Gcode_Exception with "Max token limit reached";
      elsif After = Tokens.Last - 1 then
         Append (Tokens, Tok);
      else
         for Index in reverse After + 1 .. Tokens.Last - 1 loop
            Tokens.Tokens (Index + 1) := Tokens.Tokens (Index);
         end loop;
         Tokens.Tokens (After + 1) := Tok;
         Tokens.Last := Tokens.Last + 1;
      end if;
   end Insert;

   ---------
   -- Get --
   ---------

   function Get (Tokens : Token_List; Pos : Token_Range) return Token is
   begin
      if Pos > Tokens.Last - 1 then
         return (0, 0, Unknown_Token, 0.0);
      end if;
      return Tokens.Tokens (Pos);
   end Get;

   ---------
   -- Pop --
   ---------

   function Pop (Tokens : in out Token_List) return Token is
   begin
      if Tokens.Last = Token_Range'First then
         return (0, 0, Unknown_Token, 0.0);
      else
         Tokens.Last := Tokens.Last - 1;
         return Tokens.Tokens (Tokens.Last);
      end if;
   end Pop;

   ---------
   -- Pop --
   ---------

   procedure Pop (Tokens : in out Token_List) is
   begin
      if Tokens.Last > Token_Range'First then
         Tokens.Last := Tokens.Last - 1;
      end if;
   end Pop;

   ---------
   -- Top --
   ---------

   function Top (Tokens : Token_List) return Token is
   begin
      if Tokens.Last = Token_Range'First then
         return (0, 0, Unknown_Token, 0.0);
      else
         return Tokens.Tokens (Tokens.Last - 1);
      end if;
   end Top;

   ---------------------
   -- Number_Of_Token --
   ---------------------

   function Number_Of_Token (Tokens : Token_List) return Natural is
   begin
      return Tokens.Last - 1;
   end Number_Of_Token;

   -----------
   -- Clear --
   -----------

   procedure Clear (Tokens : in out Token_List) is
   begin
      Tokens.Last := Token_Range'First;
   end Clear;

   -----------
   -- Print --
   -----------

   procedure Print (Ctx : in out Gcontext; Tokens : Token_List) is
   begin
      if Tokens.Last = Token_Range'First then
         Ctx.Put_Line ("No Token");
      else
         for Index in Token_Range'First .. Tokens.Last - 1 loop
            Print (Ctx, Tokens.Tokens (Index));
         end loop;
         Ctx.New_Line;
      end if;
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print (Ctx : in out Gcontext; Tok : Token) is
   begin
      Ctx.Put ("(" & Tok.Tstart'Img & "," & Tok.Tend'Img & ", " &
             Tok.Ttype'Img);
      if Tok.Ttype = Literal then
         Ctx.Put ("," & Tok.Value'Img);
      end if;
      Ctx.Put (")");
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print (Ctx : in out GContext; Tokens : Token_List; Line : String) is
   begin
      if Tokens.Last = Token_Range'First then
         Ctx.Put_Line ("No Token");
      else
         for Index in Token_Range'First .. Tokens.Last - 1 loop
            Print (Ctx, Tokens.Tokens (Index), Line);
         end loop;
         Ctx.New_Line;
      end if;
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print (Ctx : in out Gcontext; Tok : Token; Line : String) is
   begin
      Print (Ctx, Tok);
      Ctx.New_Line;
      Ctx.Put_Line (Line);
      for Index in Line'First .. Tok.Tstart - 1 loop
         GContext'Class (Ctx).Put (' ');
      end loop;
      GContext'Class (Ctx).Put ('^');
      if Tok.Tstart /= Tok.Tend then
         for Index in Tok.Tstart + 1 .. Tok.Tend - 1 loop
            GContext'Class (Ctx).Put ('-');
         end loop;
         GContext'Class (Ctx).Put ('^');
      end if;
      Ctx.New_Line;
   end Print;

   -----------------
   -- Eval_Number --
   -----------------

   function Eval_Number (Line : String; Tok : Token) return Float_Value is

      function Char_To_Float (C : Character) return Float_Value;

      -------------------
      -- Char_To_Float --
      -------------------

      function Char_To_Float (C : Character) return Float_Value is
      begin
         return Float_Value (Character'Pos (C) - Character'Pos ('0'));
      end Char_To_Float;

      Ret : Float_Value := 0.0;
      Cnt : Natural := 0;
      Floating : Boolean := False;
   begin
      for Index in reverse Tok.Tstart .. Tok.Tend loop
         case Line (Index) is
            when '.' =>
               if Floating then
                  --  Two points in a number...
                  raise Gcode_Exception with "Two points in a number...";
               end if;

               --  Ret := Ret / 10.0**Cnt; is not available...
               for Index in 1 .. Cnt loop
                  Ret := Ret / 10.0;
               end loop;
               Cnt := 0;
               Floating := True;
            when '0' .. '9' =>
               declare
                  Tmp : Float_Value := 1.0;
               begin
                  for Index in 1 .. Cnt loop
                        Tmp := Tmp * 10.0;
                  end loop;
                  Ret := Ret + Char_To_Float (Line (Index)) * Tmp;
               end;
               Cnt := Cnt + 1;
            when others =>
               --  No error handling
               raise Gcode_Exception with
                 "Invalid character in number literal";
         end case;
      end loop;
      return Ret;
   end Eval_Number;

   --------------
   -- Tokenize --
   --------------

   procedure Tokenize (Line : String;
                       Ctx : in out GContext'Class;
                       Tokens : in out Token_List)
   is
      Cursor : Natural := Line'First;

      function End_Of_Line return Boolean;
      procedure Increment;
      procedure Decrement;
      procedure Tokenize_Param_Name;
      procedure Tokenize_Number;
      procedure Tokenize_Comment;
      function Next_Char return Character;
      function Current_Char return Character;

      -----------------
      -- End_Of_Line --
      -----------------

      function End_Of_Line return Boolean is
      begin
         return Cursor not in Line'Range;
      end End_Of_Line;

      ---------------
      -- Increment --
      ---------------

      procedure Increment is
      begin
         Cursor := Cursor + 1;
      end Increment;

      ---------------
      -- Decrement --
      ---------------

      procedure Decrement is
      begin
         Cursor := Cursor - 1;
      end Decrement;

      -------------------------
      -- Tokenize_Param_Name --
      -------------------------

      procedure Tokenize_Param_Name is
         Tok : Token;
      begin
         Tok.Tstart := Cursor;
         Tok.Ttype := Param_Name;
         if Line (Cursor) /= '<' then
            Ctx.Report_Error (Line   => Line,
                              Msg    => "'<' expected",
                              EStart => Cursor);
            return;
         end if;
         loop
            Increment;
            exit when End_Of_Line or else Line (Cursor) = '>';
         end loop;
         if End_Of_Line then
            Ctx.Report_Error (Line   => Line,
                              Msg    => "Unmatched '<'",
                              EStart => Tok.Tstart);
            return;
         end if;
         if Cursor = Tok.Tstart + 1 then
            Ctx.Report_Error (Line   => Line,
                              Msg    => "Empty parameter name",
                              EStart => Cursor);
            return;
         end if;
         Tok.Tend := Cursor;
         Append (Tokens, Tok);
      end Tokenize_Param_Name;

      ---------------------
      -- Tokenize_Number --
      ---------------------

      procedure Tokenize_Number is
         Tok : Token;
      begin
         Tok.Tstart := Cursor;
         Tok.Ttype := Literal;
         while not End_Of_Line and then
           (Line (Cursor) in '0' .. '9' or else Line (Cursor) = '.')
         loop
            Increment;
         end loop;
         if Cursor = Tok.Tstart then
            Ctx.Report_Error (Line   => Line,
                              Msg    => "Literal expected",
                              EStart => Cursor);
            return;
         end if;
         Tok.Tend := Cursor - 1;
         Tok.Value := Eval_Number (Line, Tok);
         Append (Tokens, Tok);
         Decrement;
      end Tokenize_Number;

      ----------------------
      -- Tokenize_Comment --
      ----------------------

      procedure Tokenize_Comment is
         Tok : Token;
      begin
         Tok.Tstart := Cursor + 1;
         Tok.Ttype := Comment;
         if Line (Cursor) = ';' then
            Tok.Tend := Line'Last;
            Cursor := Line'Last;
            Append (Tokens, Tok);
            return;
         end if;
         loop
            Increment;
            exit when End_Of_Line or else Line (Cursor) = ')';
            if Line (Cursor) = '(' then
               Ctx.Report_Error (Line   => Line,
                                 Msg    => "Neested parenteses not supported",
                                 EStart => Cursor);
               return;
            end if;
         end loop;
         if End_Of_Line then
            Ctx.Report_Error (Line   => Line,
                              Msg    => "Unmatched Comment parenteses",
                              EStart => Tok.Tstart,
                              EEnd   => Cursor);
            return;
         end if;
         Tok.Tend := Cursor - 1;
         Append (Tokens, Tok);
      end Tokenize_Comment;

      ---------------
      -- Next_Char --
      ---------------

      function Next_Char return Character is
      begin
         if Cursor + 1 not in Line'Range then
            return ASCII.NUL;
         else
            return Line (Cursor + 1);
         end if;
      end Next_Char;

      ------------------
      -- Current_Char --
      ------------------

      function Current_Char return Character is
      begin
         if Cursor not in Line'Range then
            return ASCII.NUL;
         else
            return Line (Cursor);
         end if;
      end Current_Char;

   begin
      while not End_Of_Line loop
         case Current_Char is
            when ' ' => null;
            when '#' => Append (Tokens, Param, Cursor);
            when '<' => Tokenize_Param_Name;
            when '>' =>
               Ctx.Report_Error (Line   => Line,
                                 Msg    => "Unmatched '>'",
                                 EStart => Cursor);
               return;
            when '0' .. '9'  | '.' => Tokenize_Number;
            when '=' => Append (Tokens, Assign, Cursor);
            when 'N' | 'n' =>  Append (Tokens, Line_Number, Cursor);
            when 'A' .. 'M' | 'O' .. 'Z' | 'a' .. 'm' | 'o' .. 'z' =>
               Append (Tokens, Word, Cursor);
            when '[' => Append (Tokens, Expr_Start, Cursor);
            when ']' => Append (Tokens, Expr_End, Cursor);
            when '+' => Append (Tokens, Op_Plus, Cursor);
            when '-' => Append (Tokens, Op_Minus, Cursor);
            when '(' | ';' =>  Tokenize_Comment;
            when ')' =>
               Ctx.Report_Error (Line,
                                 "Unmatched Comment parenteses",
                                 Cursor);
               return;
            when '*' =>
               if Next_Char = '*' then
                  Append (Tokens, Op_Power, Cursor, Cursor + 1);
                  Increment;
               else
                  Append (Tokens, Op_Mul, Cursor);
               end if;
            when '/' => Append (Tokens, Op_Div, Cursor);
            when ASCII.LF | ASCII.CR =>
               exit;
            when others =>
               Ctx.Report_Error (Line, "Unknown Character '" & Line (Cursor) &
                                   "'", Cursor);
               return;
         end case;
         Increment;
      end loop;
      Append (Tokens, End_Of_Line, Cursor);
   end Tokenize;
end Gcode.Lexer;
