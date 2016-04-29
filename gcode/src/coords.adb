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

package body Coords is

   -----------
   -- "abs" --
   -----------

   function "abs" (A : Position) return Position is
      Ret : Position;
   begin
      for Index in Index_Type loop
         Ret (Index) := abs A (Index);
      end loop;
      return Ret;
   end "abs";

   ---------
   -- "+" --
   ---------

   function "+" (A, B : Position) return Position is
      Ret : Position;
   begin
      for Index in Index_Type loop
         Ret (Index) := A (Index)  + B (Index);
      end loop;
      return Ret;
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (A, B : Position) return Position is
      Ret : Position;
   begin
      for Index in Index_Type loop
         Ret (Index) := A (Index) - B (Index);
      end loop;
      return Ret;
   end "-";

   ---------
   -- "+" --
   ---------

   function "+" (A : Position; B : Component) return Position is
      Ret : Position;
   begin
      for Index in Index_Type loop
         Ret (Index) := A (Index) + B;
      end loop;
      return Ret;
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (A : Position; B : Component) return Position is
      Ret : Position;
   begin
      for Index in Index_Type loop
         Ret (Index) := A (Index) - B;
      end loop;
      return Ret;
   end "-";

   ---------
   -- "+" --
   ---------

   function "+" (A : Component; B : Position) return Position is
      Ret : Position;
   begin
      for Index in Index_Type loop
         Ret (Index) := B (Index) + A;
      end loop;
      return Ret;
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (A : Component; B : Position) return Position is
      Ret : Position;
   begin
      for Index in Index_Type loop
         Ret (Index) := B (Index) - A;
      end loop;
      return Ret;
   end "-";

   ---------
   -- "/" --
   ---------

   function "/" (A : Position; B : Component) return Position is
      Ret : Position;
   begin
      for Index in Index_Type loop
         Ret (Index) := A (Index) / B;
      end loop;
      return Ret;
   end "/";

end Coords;
