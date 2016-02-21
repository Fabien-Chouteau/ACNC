with Ada.Containers.Doubly_Linked_Lists;
with Stepper; use Stepper;

package body Machine_Motion_history is

   use type Step_Position;

   package Position_Container is
     new Ada.Containers.Doubly_Linked_Lists (Step_Position);
   use Position_Container;

   procedure Set_Step_Direction (Axis : Axis_Name;
                                 Dir : Direction);
   procedure Clear_Step_Pin (Axis : Axis_Name);
   procedure Set_Step_Pin (Axis : Axis_Name);

   -----------------
   -- Machine_Sim --
   -----------------

   protected Machine_Sim is
      procedure Set_Step_Direction (Axis : Axis_Name;
                                    Dir : Direction);
      procedure Clear_Step_Pin (Axis : Axis_Name);
      procedure Set_Step_Pin (Axis : Axis_Name);
      function Current_Position return Gcode.Step_Position;
      procedure Draw_History (Cr : Cairo_Context; Zoom : Gdouble);
      procedure Clear_History;
   private
      History : Position_Container.List;
      Current_Pos : Step_Position := (others => 0);
      Current_Dir : Axis_Directions := (others => Forward);
   end Machine_Sim;

   protected body Machine_Sim is
      ------------------
      -- Set_Step_Pin --
      ------------------

      procedure Set_Step_Pin (Axis : Axis_Name) is
         S : constant Steps :=
           (if Current_Dir (Axis) = Forward then 1 else -1);
      begin
         Current_Pos (Axis) := Current_Pos (Axis) + S;

         --  Do not Save Z_Axis
         if Axis /= Z_Axis then
            History.Append (Current_Position);
         end if;
      end Set_Step_Pin;

      --------------------
      -- Clear_Step_Pin --
      --------------------

      procedure Clear_Step_Pin (Axis : Axis_Name) is
      begin
         null;
      end Clear_Step_Pin;

      ------------------------
      -- Set_Step_Direction --
      ------------------------

      procedure Set_Step_Direction (Axis : Axis_Name;
                                    Dir : Direction)
      is
      begin
         Current_Dir (Axis) := Dir;
      end Set_Step_Direction;

      ----------------------
      -- Current_Position --
      ----------------------

      function Current_Position return Gcode.Step_Position is
      begin
         return Current_Pos;
      end Current_Position;

      ------------------
      -- Draw_History --
      ------------------

      procedure Draw_History (Cr : Cairo_Context; Zoom : Gdouble) is
      begin
         for Pos of History loop
            if Pos (Z_Axis) > 0 then
               Set_Source_Rgb (Cr, 0.0, 1.0, 0.0);
            else
               Set_Source_Rgb (Cr, 1.0, 0.0, 0.0);
            end if;
            Rectangle (Cr     => Cr,
                       X      => Gdouble (Pos (X_Axis)) - 1.0 * (1.0 / Zoom),
                       Y      => Gdouble (Pos (Y_Axis)) - 1.0 * (1.0 / Zoom),
                       Width  => 2.0 * (1.0 / Zoom),
                       Height => 2.0 * (1.0 / Zoom));
            Cairo.Fill (Cr);
            --  Line_To (Cr, Gdouble (Pos (X_Axis)), Gdouble (Pos (Y_Axis)));
         end loop;
         --        Cairo.Stroke (Cr);
      end Draw_History;

      -------------------
      -- Clear_History --
      -------------------

      procedure Clear_History is
      begin
         History.Clear;
      end Clear_History;

   end Machine_Sim;

   ------------------
   -- Set_Step_Pin --
   ------------------

   procedure Set_Step_Pin (Axis : Axis_Name) is
   begin
      Machine_Sim.Set_Step_Pin (Axis);
   end Set_Step_Pin;

   --------------------
   -- Clear_Step_Pin --
   --------------------

   procedure Clear_Step_Pin (Axis : Axis_Name) is
   begin
      Machine_Sim.Clear_Step_Pin (Axis);
   end Clear_Step_Pin;

   ------------------------
   -- Set_Step_Direction --
   ------------------------

   procedure Set_Step_Direction (Axis : Axis_Name;
                                 Dir : Direction)
   is
   begin
      Machine_Sim.Set_Step_Direction (Axis, Dir);
   end Set_Step_Direction;

   ----------------------
   -- Current_Position --
   ----------------------

   function Current_Position return Gcode.Step_Position is
     (Machine_Sim.Current_Position);

   ------------------
   -- Draw_History --
   ------------------

   procedure Draw_History (Cr : Cairo_Context; Zoom : Gdouble) is
   begin
      Machine_Sim.Draw_History (Cr, Zoom);
   end Draw_History;

   procedure Clear_History is
   begin
      Machine_Sim.Clear_History;
   end Clear_History;

   -----------------
   -- Stepper_Sim --
   -----------------

   task Stepper_Sim is
   end Stepper_Sim;

   task body Stepper_Sim is
   begin
      loop
         --  Run as fast as possible until there's no more segments
         while Stepper.Execute_Step_Event loop
            null;
         end loop;

         delay 0.1;
      end loop;
   end Stepper_Sim;

begin
   Stepper.Set_Stepper_Callbacks (Set_Step       => Set_Step_Pin'Access,
                                  Clear_Step     => Clear_Step_Pin'Access,
                                  Set_Direcetion => Set_Step_Direction'Access);

end Machine_Motion_history;
