package STM32.LCDInit is

   type LCD_Orientation is
     (Portrait,
      Landscape);

   LCD_WIDTH  : constant := 800;
   LCD_HEIGHT : constant := 480;
   VSYNC      : constant := 12;
   VBP        : constant := 12;
   VFP        : constant := 12;
   HSYNC      : constant := 120;
   HBP        : constant := 120;
   HFP        : constant := 120;

   Default_Orientation : constant LCD_Orientation := Landscape;

   procedure Pre_LTDC_Initialize (Orientation : LCD_Orientation);

   procedure Post_LTDC_Initialize (Orientation : LCD_Orientation);

   procedure Default_Preinit;
   procedure Default_Postinit;

end STM32.LCDInit;
