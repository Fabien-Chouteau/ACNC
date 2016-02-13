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
