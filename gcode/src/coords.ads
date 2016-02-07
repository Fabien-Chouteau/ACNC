generic
   type Component is private;
   Init_Val : Component;
   with function "+" (X, Y : Component) return Component is <>;
   with function "-" (X, Y : Component) return Component is <>;
   with function "/" (X, Y : Component) return Component is <>;
   with function "abs" (X : Component) return Component is <>;
package Coords is

   type Position is record
      X, Y, Z : Component := Init_Val;
   end record;

   function "abs" (A : Position) return Position is (abs A.X, abs A.Y, abs A.Z);
   function "+" (A, B : Position) return Position is
     ((A.X + B.X), (A.Y + B.Y), (A.Z + B.Z));
   function "-" (A, B : Position) return Position is
     ((A.X - B.X), (A.Y - B.Y), (A.Z - B.Z));
   function "+" (A : Position; B : Component) return Position is
     ((A.X + B), (A.Y + B), (A.Z + B));
   function "-" (A : Position; B : Component) return Position is
     ((A.X - B), (A.Y - B), (A.Z - B));
   function "+" (A : Component; B : Position) return Position is
     ((A + B.X), (A + B.Y), (A + B.Z));
   function "-" (A : Component; B : Position) return Position is
     ((A - B.X), (A - B.Y), (A - B.Z));
   function "/" (A : Position; B : Component) return Position is
     ((A.X / B), (A.Y / B), (A.Z / B));
end Coords;
