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

with Gcode.Context; use Gcode.Context;
package Gcode.Lexer is

   type Token_Type is (Unknown_Token, Param, Param_Name, Expr_Start, Expr_End,
                       Op_Plus, Op_Minus, Op_Uni_Minus, Op_Mul, Op_Div,
                       Op_Power, Word, Assign, Line_Number, Literal, Comment,
                       End_Of_Line, End_Of_Expression);
   type Token is record
      Tstart, Tend : Natural := 0;
      Ttype : Token_Type := Unknown_Token;
      Value : Float_Value;
   end record;

   Max_Tokens : constant := 30;
   subtype Token_Range is Natural range 1 .. Max_Tokens;
   type Token_Array is array (Token_Range) of Token;

   type Token_List is record
      Tokens : Token_Array;
      Last   : Natural := Token_Range'First;
   end record;

   procedure Clear (Tokens : in out Token_List);
   procedure Append (Tokens : in out Token_List; Tok : Token);
   procedure Append (Tokens : in out Token_List;
                     Ttype : Token_Type;
                     Tstart, Tend : Natural;
                     Value  : Float_Value := 0.0);
   procedure Append (Tokens : in out Token_List;
                     Ttype : Token_Type;
                     Position : Natural;
                     Value  : Float_Value := 0.0);
   procedure Insert (Tokens : in out Token_List; Tok : Token;
                     After : Token_Range);
   procedure Push (Tokens : in out Token_List; Tok : Token) renames Append;
   function Top (Tokens : Token_List) return Token;
   function Pop (Tokens : in out Token_List) return Token;
   procedure Pop (Tokens : in out Token_List);
   function Get (Tokens : Token_List; Pos : Token_Range) return Token;
   function Number_Of_Token (Tokens : Token_List) return Natural;

   procedure Print (Ctx : in out Gcontext; Tokens : Token_List);
   procedure Print (Ctx : in out Gcontext; Tok : Token);
   procedure Print (Ctx : in out Gcontext; Tokens : Token_List; Line : String);
   procedure Print (Ctx : in out Gcontext; Tok : Token; Line : String);

   procedure Tokenize (Line   : String;
                       Ctx    : in out GContext'Class;
                       Tokens : in out Token_List);
end Gcode.Lexer;
