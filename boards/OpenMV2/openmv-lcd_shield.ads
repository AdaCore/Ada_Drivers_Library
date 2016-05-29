package OpenMV.LCD_Shield is
   Width  : constant := 128;
   Height : constant := 160;

   procedure Initialize;
   function Initialized return Boolean;

   procedure Display;
end OpenMV.LCD_Shield;
