with Ada.Text_IO; use Ada.Text_IO;
with Gcode.Parameters; use Gcode.Parameters;
with Ada.Numerics.Generic_Elementary_Functions;

package body Gcode.Shunting_Yard is

   package Float_Functions is new
     Ada.Numerics.Generic_Elementary_Functions (Long_Float);

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
                        Output : in out Token_List) return Long_Float is
      Tok : constant Token := Pop (Output);
      R1, R2 : Long_Float;
   begin
      case Tok.Ttype is
         when Op_Plus =>
            R1 := Eval_Stack(Line, Ctx, Output);
            R2 := Eval_Stack(Line, Ctx, Output);
            return R2 + R1;
         when Op_Uni_Minus =>
            R1 := Eval_Stack(Line, Ctx, Output);
            return -R1;
         when Op_Minus =>
            R1 := Eval_Stack(Line, Ctx, Output);
            R2 := Eval_Stack(Line, Ctx, Output);
            return R2 - R1;
         when Op_Div =>
            R1 := Eval_Stack(Line, Ctx, Output);
            R2 := Eval_Stack(Line, Ctx, Output);
            if R1 = 0.0 then
               Put_Line ("Division by zero at " & Tok.Tstart'Img);
               return 0.0;
            else
               return R2 / R1;
            end if;
         when Op_Mul =>
            R1 := Eval_Stack(Line, Ctx, Output);
            R2 := Eval_Stack(Line, Ctx, Output);
            return R2 * R1;
         when Op_Power =>
            R1 := Eval_Stack(Line, Ctx, Output);
            R2 := Eval_Stack(Line, Ctx, Output);
            return Float_Functions."**"(R2, R1);
         when Literal =>
            return Tok.Value;
         when others =>
            Raise_Error (Ctx,
                         "Unexpected " & Tok.Ttype'Img & " at " &
                           Tok.Tstart'Img & " in expression");
            return 0.0;
      end case;
   end;

   -------------------------
   -- Evaluate_Expression --
   -------------------------

   function Evaluate_Expression (Line   : String;
                                 Ctx    : in out GContext'Class;
                                 Tokens : Token_List;
                                 Cur    : in out Token_Range)
                                 return Long_Float
   is
      Output, Op_Stack : Token_List;

      function Next (Cnt : Natural := 1)return Token is
        (Get (Tokens, Cur + Cnt));

      Previous : Token := (0, 0, Unknown_Token, 0.0);
      Current  : Token;
   begin
      while Cur < Tokens.Last loop
         Current := Get (Tokens, Cur);
         case Current.Ttype is
            when Literal => Push (Output, Current);
            when Param =>
               case Next.Ttype is
                  when Param_Name =>
                     Raise_Error (Ctx,
                                  "Named parameters are not supported yet...");
                  when Literal =>
                     declare
                        Value : constant Long_Float := Next.Value;
                        Id : constant Parameter_Id := Natural (Value);
                        Value_Tok : Token;
                     begin
                        Value_Tok.Tstart := Current.Tstart;
                        Value_Tok.Tend := Next.Tend;
                        Cur := Cur + 1;
                        if not Defined (Ctx.Params, Id) then
                           Raise_Error
                             (Ctx, "Undefined numbered parameter: " & Id'Img);
                        else
                           Value_Tok.Tend  := Cur - 1;
                           Value_Tok.Ttype := Literal;
                           Value_Tok.Value := Get_Value (Ctx.Params, Id);
                           Push (Output, Value_Tok);
                        end if;
                     end;
                  when others =>
                     Raise_Error (Ctx, "Parameter Id expected at " &
                                    Natural'Image (Current.Tend + 1));
               end case;
            when Expr_Start => Push (Op_Stack, Current);
            when Expr_End =>
               while Top (Op_Stack).Ttype /= Expr_Start and then
                 Top (Op_Stack).Ttype /= Unknown_Token
               loop
                  Push (Output, Pop (Op_Stack));
               end loop;
               if Top (Op_Stack).Ttype = Unknown_Token then
                  Raise_Error
                    (Ctx, "Unmatched right bracket at" & Current.Tstart'Img);
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
                     exit when
                         Top_Type not in Operators
                       or else
                         not ((Precedence (Cur_Type) < Precedence (Top_Type))
                              or else (Asso (Cur_Type) = Left
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
            Raise_Error
              (Ctx, "Unmatched left bracket at " & Top (Op_Stack).Tstart'Img);
            return 0.0;
         else
            Push (Output, Pop (Op_Stack));
         end if;
      end loop;
      return Eval_Stack (Line, Ctx, Output);
   end;
end Gcode.Shunting_Yard;
