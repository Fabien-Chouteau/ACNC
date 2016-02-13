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
