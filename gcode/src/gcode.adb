package body Gcode is

   --------------
   -- Distance --
   --------------

   function Distance (A, B : Float_Position) return Float_Value is
      Tmp : constant Float_Value :=
        (A.X - B.X)**2 + (A.Y - B.Y)**2 + (A.Z - B.Z)**2;

      ----------
      -- Sqrt --
      ----------

      function Sqrt (X : Float_Value) return Float_Value is
         U     : Float_Value := X;
         New_U : Float_Value;
      begin
         if X < 0.0 then
            raise Program_Error;
         end if;
         if X = 0.0 then
            return 0.0;
         end if;
         loop
            New_U := (U + X/U)/2.0;
            exit when New_U >= U;
            U := New_U;
         end loop;
         return U;
      end Sqrt;   begin
      return Sqrt (Tmp);
   exception
      when others =>
         return 0.0;
   end Distance;

   -----------
   -- Image --
   -----------

   function Image (Val : Float_Value) return String is
      Int_Part : constant Integer := Integer (Float_Value'Floor (Val));
      Frac_Part : constant Integer := Integer (Float_Value'Floor (Val * 10000.0));
   begin
      return Int_Part'Img & "." & Frac_Part'Img;
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Pos : Float_Position) return String is
   begin
      return ("X:" & Image (Pos.X) & " Y:" & Image (Pos.Y)
              & " Z:" & Image (Pos.Z));
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Pos : Step_Position) return String is
   begin
      return ("X:" & Pos.X'Img & " Y:" & Pos.Y'Img & " Z:" & Pos.Z'Img);
   end Image;

end Gcode;
