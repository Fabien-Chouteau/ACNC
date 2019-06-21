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

with Gcode.Parameters; use Gcode.Parameters;
with Ada.Numerics.Generic_Elementary_Functions;

package body Gcode.Shunting_Yard is
   function Eval_Stack (Line   : String;
                        Ctx    : in out GContext'Class;
                        Output : in out Token_List) return Float_Value;

   package Float_Functions is new
     Ada.Numerics.Generic_Elementary_Functions (Float_Value);

   type Associative is (Left, Right);
   subtype Operators is Token_Type range Op_Plus .. Op_Power;

   Asso : constant array (Operators) of Associative :=
     (Op_Minus     => Left,
      Op_Plus      => Left,
      Op_Div       => Left,
      Op_Mul       => Left,
      Op_Power     => Right,
      Op_Uni_Minus => Right);
   Precedence : constant array (Operators) of Natural :=
     (Op_Minus     => 2,
      Op_Plus      => 2,
      Op_Div       => 3,
      Op_Mul       => 3,
      Op_Power     => 4,
      Op_Uni_Minus => 5);

   ----------------
   -- Eval_Stack --
   ----------------

   function Eval_Stack (Line   : String;
                        Ctx    : in out GContext'Class;
                        Output : in out Token_List) return Float_Value is
      Tok : constant Token := Pop (Output);
      R1, R2 : Float_Value;
   begin
      case Tok.Ttype is
         when Op_Plus =>
            R1 := Eval_Stack (Line, Ctx, Output);
            R2 := Eval_Stack (Line, Ctx, Output);
            return R2 + R1;
         when Op_Uni_Minus =>
            R1 := Eval_Stack (Line, Ctx, Output);
            return -R1;
         when Op_Minus =>
            R1 := Eval_Stack (Line, Ctx, Output);
            R2 := Eval_Stack (Line, Ctx, Output);
            return R2 - R1;
         when Op_Div =>
            R1 := Eval_Stack (Line, Ctx, Output);
            R2 := Eval_Stack (Line, Ctx, Output);
            if R1 = 0.0 then
               return 0.0;
            else
               return R2 / R1;
            end if;
         when Op_Mul =>
            R1 := Eval_Stack (Line, Ctx, Output);
            R2 := Eval_Stack (Line, Ctx, Output);
            return R2 * R1;
         when Op_Power =>
            R1 := Eval_Stack (Line, Ctx, Output);
            R2 := Eval_Stack (Line, Ctx, Output);
            return Float_Functions."**"(R2, R1);
         when Literal =>
            return Tok.Value;
         when others =>
            Ctx.Report_Error (Line   => Line,
                              Msg    => "Unexpected " & Tok.Ttype'Img &
                              " in expression",
                              EStart => Tok.Tstart,
                              EEnd   => Tok.Tend);
            return 0.0;
      end case;
   end Eval_Stack;

   -------------------------
   -- Evaluate_Expression --
   -------------------------

   function Evaluate_Expression (Line   : String;
                                 Ctx    : in out GContext'Class;
                                 Tokens : Token_List;
                                 Cur    : in out Token_Range)
                                 return Float_Value
   is
      Output, Op_Stack : Token_List;

      function Next (Cnt : Natural := 1) return Token is
        (Get (Tokens, Cur + Cnt));

      Previous : Token := (0, 0, Unknown_Token, 0.0);
      Current  : Token;
   begin
      Push (Output, Tok => (Tstart => 0,
                            Tend   => 0,
                            Ttype  => End_Of_Expression,
                            Value  => 0.0));

      while Cur < Tokens.Last loop
         Current := Get (Tokens, Cur);
         case Current.Ttype is
            when Literal => Push (Output, Current);
            when Param =>
               case Next.Ttype is
                  when Param_Name =>
                     declare
                        Name : constant String :=
                          Line (Next.Tstart + 1 .. Next.Tend - 1);
                        Value_Tok : Token;
                     begin
                        Value_Tok.Tstart := Current.Tstart;
                        Value_Tok.Tend := Next.Tend;
                        Cur := Cur + 1;
                        if not Defined (Ctx.Params, Name) then
                           Ctx.Report_Error
                             (Line   => Line,
                              Msg    => "Undefined named parameter: " & Name,
                              EStart => Current.Tstart,
                              EEnd   => Current.Tend);
                        else
                           Value_Tok.Tend  := Cur - 1;
                           Value_Tok.Ttype := Literal;
                           Value_Tok.Value := Get_Value (Ctx.Params, Name);
                           Push (Output, Value_Tok);
                        end if;
                     end;
                  when Literal =>
                     declare
                        Value : constant Float_Value := Next.Value;
                        Id : constant Parameter_Id := Natural (Value);
                        Value_Tok : Token;
                     begin
                        Value_Tok.Tstart := Current.Tstart;
                        Value_Tok.Tend := Next.Tend;
                        Cur := Cur + 1;
                        if not Defined (Ctx.Params, Id) then
                           Ctx.Report_Error
                             (Line   => Line,
                              Msg    => "Undefined numbered parameter: " &
                                Id'Img,
                              EStart => Current.Tstart,
                              EEnd   => Current.Tend);
                        else
                           Value_Tok.Tend  := Cur - 1;
                           Value_Tok.Ttype := Literal;
                           Value_Tok.Value := Get_Value (Ctx.Params, Id);
                           Push (Output, Value_Tok);
                        end if;
                     end;
                  when others =>
                     Ctx.Report_Error
                       (Line   => Line,
                        Msg    => "Parameter Id expected",
                        EStart => Current.Tstart,
                        EEnd   => Current.Tend);
               end case;
            when Expr_Start => Push (Op_Stack, Current);
            when Expr_End =>
               while Top (Op_Stack).Ttype /= Expr_Start and then
                 Top (Op_Stack).Ttype /= Unknown_Token
               loop
                  Push (Output, Pop (Op_Stack));
               end loop;
               if Top (Op_Stack).Ttype = Unknown_Token then
                  Ctx.Report_Error
                    (Line   => Line,
                     Msg    => "Unmatched right bracket",
                     EStart => Current.Tstart);
               else
                  --  Remove Expr_Start
                  Pop (Op_Stack);
               end if;
            when Operators =>
               if Current.Ttype = Op_Minus and then not
                 (Previous.Ttype = Expr_End or else
                  Previous.Ttype = Literal)
               then
                  --  This is some kind of hack to handle unary minux operator,
                  --  We just modify the type of operator when minus is not
                  --  after a right paren or a literal.
                  Current.Ttype := Op_Uni_Minus;
               end if;
               loop
                  declare
                     Top_Type : constant Token_Type := Top (Op_Stack).Ttype;
                     Cur_Type : constant Token_Type := Current.Ttype;
                  begin
                     exit when Top_Type not in Operators;
                     exit when
                     not ((Precedence (Cur_Type) < Precedence (Top_Type))
                          or else
                            (Asso (Cur_Type) = Left
                             and then
                             Precedence (Cur_Type) = Precedence (Top_Type)));

                     Push (Output, Pop (Op_Stack));
                  end;
               end loop;
               Push (Op_Stack, Current);
            when others => exit;
         end case;
         if Error_Raised (Ctx) then
            return 0.0;
         end if;
         Previous := Current;
         Cur := Cur + 1;
      end loop;

      --  Move the remaining operation in the output
      while Top (Op_Stack).Ttype /= Unknown_Token loop
         if Top (Op_Stack).Ttype = Expr_Start then
            Ctx.Report_Error
              (Line   => Line,
               Msg    => "Unmatched left bracket",
               EStart => Top (Op_Stack).Tstart);
            return 0.0;
         else
            Push (Output, Pop (Op_Stack));
         end if;
      end loop;
      return Eval_Stack (Line, Ctx, Output);
   end Evaluate_Expression;
end Gcode.Shunting_Yard;
