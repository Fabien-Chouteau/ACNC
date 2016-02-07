package body Gcode.Parameters is

   ----------
   -- Find --
   ----------

   function Find (Ctx : Parameters_Set; Id : Parameter_Id) return Natural is
   begin
      for Index in Parameter_Range'First .. Ctx.Last - 1 loop
         if Ctx.Params (Index).Id = Id then
            return Index;
         end if;
      end loop;
      return Parameter_Range'First;
   end;

   ------------
   -- Define --
   ------------

   procedure Define (Ctx : in out Parameters_Set;
                     Id : Parameter_Id;
                     Value : Long_Float)
   is
      P : constant Natural := Find (Ctx, Id);
   begin
      if P in Parameter_Range then
         Ctx.Params (P).Value := Value;
      else
         if Ctx.Last not in Parameter_Range then
            --  No error handling...
            raise Program_Error;
         else
            Ctx.Params (Ctx.Last).Ptype := Numbered;
            Ctx.Params (Ctx.Last).Value := Value;
            Ctx.Params (Ctx.Last).Id    := Id;
            Ctx.Last := Ctx.Last + 1;
         end if;
      end if;
   end;

   -------------
   -- Defined --
   -------------

   function Defined (Ctx : Parameters_Set; Id :  Parameter_Id) return Boolean is
      P : constant Natural := Find (Ctx, Id);
   begin
      return P in Parameter_Range;
   end;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value (Ctx : Parameters_Set; Id :  Parameter_Id) return Long_Float is
      P : constant Natural := Find (Ctx, Id);
   begin
      if P in Parameter_Range then
         return Ctx.Params (P).Value;
      else
         return 0.0;
      end if;
   end;
end Gcode.Parameters;
