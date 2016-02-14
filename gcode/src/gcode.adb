package body Gcode is

   --------------
   -- Distance --
   --------------

   function Distance (A, B : Float_Position) return Float_Value is
      Tmp : Float_Value := 0.0;
      function Sqrt (X : Float_Value) return Float_Value;

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
            New_U := (U + (X / U)) / 2.0;
            exit when New_U >= U;
            U := New_U;
         end loop;
         return U;
      end Sqrt;
   begin
      for Axis in Axis_Name loop
         Tmp := Tmp + (A (Axis) - B (Axis))**2;
      end loop;
      return Sqrt (Tmp);
   exception
      when others =>
         return 0.0;
   end Distance;

   -----------
   -- Image --
   -----------

   function Image (Val : Float_Value) return String is
      Floor : constant Float_Value := Float_Value'Floor (Val);
      Int_Part : constant Integer := Integer (Floor);
      Frac_Part : constant Integer :=
        Integer (Float_Value'Floor ((Val - Floor) * 10000.0));
   begin
      return Int_Part'Img & "." & Frac_Part'Img;
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Pos : Float_Position) return String is
   begin
      return ("X:" & Image (Pos (X_Axis)) & " Y:" & Image (Pos (Y_Axis))
              & " Z:" & Image (Pos (Z_Axis)));
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Pos : Step_Position) return String is
   begin
      return ("X:" & Pos (X_Axis)'Img & " Y:" & Pos (Y_Axis)'Img & " Z:" &
                Pos (Z_Axis)'Img);
   end Image;

end Gcode;
