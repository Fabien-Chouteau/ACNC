package Gcode.Parameters is

   Max_Parameter : constant := 50;
   type Param_Type is (Undefined, Named, Numbered);
   type Parameters_Set is private;
   subtype Parameter_Id is Natural;

   type Parameter is record
      Ptype : Param_Type := Undefined;
      Id    : Parameter_Id;
      Value : Long_Float := 0.0;
   end record;

   Undefined_Param : constant Parameter := (Undefined, 0, 0.0);

   procedure Define (Ctx   : in out Parameters_Set;
                     Id    : Parameter_Id;
                     Value : Long_Float);
   function Defined (Ctx : Parameters_Set; Id : Parameter_Id) return Boolean;
   function Get_Value (Ctx : Parameters_Set; Id : Parameter_Id) return Long_Float;
private

   subtype Parameter_Range is Natural range 1 .. Max_Parameter;
   type Parameter_Array is array (Parameter_Range) of Parameter;

   type Parameters_Set is record
      Params : Parameter_Array;
      Last   : Natural := Parameter_Range'First;
   end record;
end Gcode.Parameters;
