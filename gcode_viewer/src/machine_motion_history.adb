with Ada.Real_Time; use Ada.Real_Time;
with Ada.Containers.Doubly_Linked_Lists;
with Stepper; use Stepper;
with Settings;

package body Machine_Motion_history is

   use type Step_Position;

   package Position_Container is
     new Ada.Containers.Doubly_Linked_Lists (Step_Position);
   use Position_Container;

   procedure Set_Step_Direction (Axis : Axis_Name;
                                 Dir : Direction);
   procedure Clear_Step_Pin (Axis : Axis_Name);
   procedure Set_Step_Pin (Axis : Axis_Name);
   procedure Set_Stepper_Frequency (Freq_Hz : Frequency_Value);
   function Home_Test (Axis : Axis_Name) return Boolean;
   procedure Motor_Enable (Axis : Axis_Name;
                           Enable : Boolean);

   Task_Period : Time_Span := Milliseconds (500);
   Time_Factor : Integer := 10;
   Motor_Enabled : Motor_Enable_Array := (others => False);

   type Save_Mod is mod 5;

   -----------------
   -- Machine_Sim --
   -----------------

   protected Machine_Sim is
      procedure Set_Step_Direction (Axis : Axis_Name;
                                    Dir : Direction);
      procedure Make_Step (Axis : Axis_Name);
      function Current_Position return Gcode.Step_Position;
      procedure Draw_History (Cr : Cairo_Context; Zoom : Gdouble);
      procedure Clear_History;
   private
      History : Position_Container.List;
      Current_Pos : Step_Position := (others => 0);
      Current_Dir : Axis_Directions := (others => Forward);

      Save : Save_Mod := 0;
   end Machine_Sim;

   protected body Machine_Sim is
      ---------------
      -- Make_Step --
      ---------------

      procedure Make_Step (Axis : Axis_Name) is
         S : constant Steps :=
           (if Current_Dir (Axis) = Forward then 1 else -1);
      begin
         Current_Pos (Axis) := Current_Pos (Axis) + S;

         --  Do not Save Z_Axis motion
         if Axis /= Z_Axis then
            if Save = 0 then
               History.Append (Current_Position);
            end if;
            Save := Save + 1;
         end if;
      end Make_Step;

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
         Z_Threshold : constant Integer :=
           Milli_To_Step ((0.0, 0.0, -13.0))(Z_Axis);
      begin
         for Pos of History loop
            if Pos (Z_Axis) > Z_Threshold then
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
      if Motor_Enabled (Axis) then
         Machine_Sim.Make_Step (Axis);
      end if;
   end Set_Step_Pin;

   --------------------
   -- Clear_Step_Pin --
   --------------------

   procedure Clear_Step_Pin (Axis : Axis_Name) is
      pragma Unreferenced (Axis);
   begin
      --  This is not required for simulation
      null;
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

   ---------------------------
   -- Set_Stepper_Frequency --
   ---------------------------

   procedure Set_Stepper_Frequency (Freq_Hz : Frequency_Value) is
   begin
      Task_Period := To_Time_Span (1.0 / (Freq_Hz / 10));
   end Set_Stepper_Frequency;

   ---------------
   -- Home_Test --
   ---------------

   function Home_Test (Axis : Axis_Name) return Boolean is
   begin
      if Axis = Z_Axis then
         return Machine_Sim.Current_Position (Axis) > 0;
      else
         return Machine_Sim.Current_Position (Axis) < 0;
      end if;
   end Home_Test;

   ------------------
   -- Motor_Enable --
   ------------------

   procedure Motor_Enable (Axis : Axis_Name;
                           Enable : Boolean) is
   begin
      Motor_Enabled (Axis) := Enable;
   end Motor_Enable;

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

   -------------------
   -- Clear_History --
   -------------------

   procedure Clear_History is
   begin
      Machine_Sim.Clear_History;
   end Clear_History;

   ---------------------
   -- Set_Time_Factor --
   ---------------------

   procedure Set_Time_Factor (Factor : Integer) is
   begin
      Time_Factor := Factor;
   end Set_Time_Factor;

   -----------------
   -- Stepper_Sim --
   -----------------

   task Stepper_Sim is
   end Stepper_Sim;

   task body Stepper_Sim is
      Next_Period : Time := Clock;
   begin
      loop
         Next_Period := Next_Period + Task_Period;

         for Cnt in 1 .. Time_Factor * 10 loop
            if Stepper.Execute_Step_Event then
               null;
            end if;
         end loop;

         delay until Next_Period;
      end loop;
   end Stepper_Sim;

begin
   Set_Stepper_Frequency (Settings.Idle_Stepper_Frequency);

   Stepper.Set_Stepper_Callbacks
     (Set_Step              => Set_Step_Pin'Access,
      Clear_Step            => Clear_Step_Pin'Access,
      Set_Direcetion        => Set_Step_Direction'Access,
      Set_Stepper_Frequency => Set_Stepper_Frequency'Access,
      Home_Test             => Home_Test'Access,
      Motor_Enable          => Motor_Enable'Access);

end Machine_Motion_history;
