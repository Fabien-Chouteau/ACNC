generic
   Height, Width : Natural;
package Virtual_Pad is
   subtype Height_Range is Natural range 1 .. Height;
   subtype Width_Range is Natural range 1 .. Width;

   procedure Create_Windows;

--     procedure Set_Light (X : Width_Range; Y : Height_Range);
end Virtual_Pad;
