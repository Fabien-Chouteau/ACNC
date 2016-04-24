with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.Bounded_Buffers;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

package body Serial_Coms is

   package Unbounded_String_Queue is new GNAT.Bounded_Buffers
     (Ada.Strings.Unbounded.Unbounded_String);

   RX : Unbounded_String_Queue.Bounded_Buffer
     (512, Unbounded_String_Queue.Default_Ceiling);
   TX : Unbounded_String_Queue.Bounded_Buffer
     (512, Unbounded_String_Queue.Default_Ceiling);

   Serial : aliased Serial_Port;

   function Open (Name : Port_Name; Baud : Data_Rate) return Boolean is
   begin
      Open (Serial, Name);
      Set (Serial,
           Baud,
           Block   => False,
           Flow    => RTS_CTS,
           Timeout => 0.01);
      return True;
   exception
      when Serial_Error =>
         return False;
   end Open;

   ----------
   -- Send --
   ----------

   procedure Send (Data : String) is
   begin
      TX.Insert (To_Unbounded_String (Data));
   end Send;

   --------------
   -- RX_Empty --
   --------------

   function RX_Empty return Boolean is (RX.Empty);

   ---------
   -- Get --
   ---------

   function Get return String is
   begin
      if RX_Empty then
         return "";
      else
         declare
            Ret : Unbounded_String;
         begin
            RX.Remove (Ret);
            return To_String (Ret);
         end;
      end if;
   end Get;

   -------------
   -- TX_Task --
   -------------

   task TX_Task is
   end TX_Task;

   -------------
   -- TX_Task --
   -------------

   task body TX_Task is
      Ub : Unbounded_String;
   begin
      loop
         TX.Remove (Ub);
         declare
            Data : constant String := To_String (Ub);
         begin
            Put_Line ("Sending: '" & Data & "'");
            String'Write (Serial'Access, Data);
            Put_Line ("Sent: '" & Data & "'");
         end;
      end loop;
   exception
      when E : others =>
         Put_Line ("TX_Task: Unknown Error: " & Exception_Message (E));
   end TX_Task;

   -------------
   -- RX_Task --
   -------------

   task RX_Task is
   end RX_Task;

   -------------
   -- RX_Task --
   -------------

   task body RX_Task is
      Ub : Unbounded_String;
      Data : String (1 .. 1);
   begin
      loop
         declare
         begin
            String'Read (Serial'Access, Data);
            if Data (1) = ASCII.CR or else Data (1) = ASCII.LF then
               RX.Insert (Ub);
               Set_Unbounded_String (Ub, "");
            else
               Ub := Ub & Data;
            end if;
         exception
            when Serial_Error =>
               --  Not connected
               delay 2.0;
            when End_Error =>
               --  Timeout
               null;
         end;
      end loop;
   exception
      when E : others =>
         Put_Line ("RX_Task: Unknown Error: " & Exception_Information (E));
   end RX_Task;

end Serial_Coms;
