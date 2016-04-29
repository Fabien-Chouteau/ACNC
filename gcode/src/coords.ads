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

generic
   type Component is private;
   type Index_Type is (<>);
   with function "+" (X, Y : Component) return Component is <>;
   with function "-" (X, Y : Component) return Component is <>;
   with function "/" (X, Y : Component) return Component is <>;
   with function "abs" (X : Component) return Component is <>;
package Coords is

   type Position is array (Index_Type) of Component;

   function "abs" (A : Position) return Position;
   function "+" (A, B : Position) return Position;
   function "-" (A, B : Position) return Position;
   function "+" (A : Position; B : Component) return Position;
   function "-" (A : Position; B : Component) return Position;
   function "+" (A : Component; B : Position) return Position;
   function "-" (A : Component; B : Position) return Position;
   function "/" (A : Position; B : Component) return Position;
end Coords;
