package Gcode.Commands is
   type Word is record
      Value  : Float;
      Is_Set : Boolean;
   end record;

   type Block is record
      G : Word;
      M : Word;
      X : Word;
      Y : Word;
      Z : Word;
      A : Word;
   end record;

end Gcode.Commands;
