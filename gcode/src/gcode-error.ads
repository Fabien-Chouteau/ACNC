package Gcode.Error is
   type Error_Flag is (Low, Raised);

   Gcode_Exception : exception;
end Gcode.Error;
