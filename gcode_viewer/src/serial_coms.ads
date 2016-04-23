with GNAT.Serial_Communications; use GNAT.Serial_Communications;

package Serial_Coms is
   type Serial_Port_Ref is access all GNAT.Serial_Communications.Serial_Port;

   function Open (Name : Port_Name; Baud : Data_Rate) return Boolean;
   procedure Send (Data : String);
   function RX_Empty return Boolean;
   function Get return String;
end Serial_Coms;
