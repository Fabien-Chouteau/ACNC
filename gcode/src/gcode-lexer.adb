with Ada.Text_IO; use Ada.Text_IO;
with Gcode.Error; use Gcode.Error;

package body Gcode.Lexer is

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
   end;

   ------------
   -- Append --
   ------------

   procedure Append (Tokens : in out Token_List;
                     Ttype : Token_Type;
                     Tstart, Tend : Natural;
                     Value  : Long_Float := 0.0)
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
                     Value  : Long_Float := 0.0)
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
   end;

   ---------
   -- Get --
   ---------

   function Get (Tokens : Token_List; Pos : Token_Range) return Token is
   begin
      if Pos > Tokens.Last - 1 then
         return (0, 0, Unknown_Token, 0.0);
      end if;
      return Tokens.Tokens (Pos);
   end;

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
   end;

   ---------
   -- Pop --
   ---------

   procedure Pop (Tokens : in out Token_List) is
   begin
      if Tokens.Last > Token_Range'First then
         Tokens.Last := Tokens.Last - 1;
      end if;
   end;

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
   end;

   ---------------------
   -- Number_Of_Token --
   ---------------------

   function Number_Of_Token (Tokens : Token_List) return Natural is
   begin
      return Tokens.Last - 1;
   end;

   -----------
   -- Clear --
   -----------

   procedure Clear (Tokens : in out Token_List) is
   begin
      Tokens.Last := Token_Range'First;
   end;

   -----------
   -- Print --
   -----------

   procedure Print (Tokens : Token_List) is
   begin
      if Tokens.Last = Token_Range'First then
         Put_Line ("No Token");
      else
         for Index in Token_Range'First .. Tokens.Last - 1 loop
            Print (Tokens.Tokens (Index));
         end loop;
         New_Line;
      end if;
   end;

   -----------
   -- Print --
   -----------

   procedure Print (Tok : Token) is
   begin
      Put ("(" & Tok.Tstart'Img & "," & Tok.Tend'Img & ", " &
             Tok.Ttype'Img);
--        if Tok.Ttype = Literal then
--           Put ("," & Tok.Value'Img);
--        end if;
      Put (")");
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print (Tokens : Token_List; Line : String) is
   begin
      if Tokens.Last = Token_Range'First then
         Put_Line ("No Token");
      else
         for Index in Token_Range'First .. Tokens.Last - 1 loop
            Print (Tokens.Tokens (Index), Line);
         end loop;
         New_Line;
      end if;
   end;

   -----------
   -- Print --
   -----------

   procedure Print (Tok : Token; Line : String) is
   begin
      Print (Tok);
      New_Line;
      Put_Line (Line);
      for Index in Line'First .. Tok.Tstart - 1 loop
         Put (' ');
      end loop;
      Put ('^');
      if Tok.Tstart /= Tok.Tend then
         for Index in Tok.Tstart + 1 .. Tok.Tend - 1 loop
            Put ('-');
         end loop;
         Put ('^');
      end if;
      New_Line;
   end Print;

   -----------------
   -- Eval_Number --
   -----------------

   function Eval_Number (Line : String; Tok : Token) return Long_Float is

      -------------------
      -- Char_To_Float --
      -------------------

      function Char_To_Float (C : Character) return Long_Float is
      begin
         return Long_Float (Character'Pos (C) - Character'Pos ('0'));
      end;
      Ret : Long_Float := 0.0;
      Cnt : Natural := 0;
      Floating : Boolean := False;
   begin
      for Index in reverse Tok.Tstart .. Tok.Tend loop
         case Line (Index) is
            when '.' =>
               if Floating then
                  --  Two points in a number...
                  raise Program_Error;
               end if;

               --  Ret := Ret / 10.0**Cnt; is not available...
               for Index in 1 .. Cnt loop
                  Ret := Ret / 10.0;
               end loop;
               Cnt := 0;
               Floating := True;
            when '0' .. '9' =>
               declare
                  Tmp : Long_Float := 1.0;
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

      function End_Of_Line return Boolean is
      begin
         return Cursor not in Line'Range;
      end;

      procedure Increment is
      begin
         Cursor := Cursor + 1;
      end Increment;
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
            Raise_Error (Ctx, "'<' expected at " & Cursor'Img);
            return;
         end if;
         loop
            Increment;
            exit when End_Of_Line or else Line (Cursor) = '>';
         end loop;
         if End_Of_Line then
            Raise_Error (Ctx, "Unmatched '<' at " & Tok.Tstart'Img);
            return;
         end if;
         if Cursor = Tok.Tstart + 1 then
            Raise_Error (Ctx, "Empty parameter name at " & Cursor'Img);
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
           (Line (Cursor) in '0' .. '9' or else Line (Cursor) = '.' )
         loop
            Increment;
         end loop;
         if Cursor = Tok.Tstart then
            Raise_Error (Ctx, "Literal expected at " & Cursor'Img);
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
            if Line (Cursor) = '(' then
               Raise_Error (Ctx,
                            "Neested parenteses not supported at " &
                              Cursor'Img);
               return;
            end if;
            exit when End_Of_Line or else Line (Cursor) = ')';
         end loop;
         if End_Of_Line then
            Raise_Error (Ctx,
                         "Unmatched Comment parenteses ar " & Tok.Tstart'Img);
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
               Ctx.Report_Error (Line, "Unmatched '>' at " & Cursor'Img,
                                 Cursor, Cursor);
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
               Ctx.Report_Error (Line, "Unmatched right paren at " & Cursor'Img,
                                 Cursor, Cursor);
               return;
            when '*' =>
               if Next_Char = '*' then
                  Append (Tokens, Op_Power, Cursor, Cursor + 1);
                  Increment;
               else
                  Append (Tokens, Op_Mul, Cursor);
               end if;
            when '/' => Append (Tokens, Op_Div, Cursor);
            when ASCII.LF | ASCII.CR => return;
            when others =>
               Ctx.Report_Error (Line, "Unknown Character '" & Line (Cursor) &
                                   "'", Cursor, Cursor);
               return ;
         end case;
         Increment;
      end loop;
   end;
end Gcode.Lexer;
