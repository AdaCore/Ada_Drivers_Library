with STM32.DSI;

package STM32.LCDInit is

   type LCD_Orientation is
     (Portrait,
      Landscape);
   Default_Orientation : constant LCD_Orientation := Landscape;

   LCD_WIDTH  : constant := 800;
   LCD_HEIGHT : constant := 480;
   VSYNC      : constant := 12;
   VBP        : constant := 12;
   VFP        : constant := 12;
   HSYNC      : constant := 120;
   HBP        : constant := 120;
   HFP        : constant := 120;

   PLLSAIN     : constant := 384;
   PLLSAIR     : constant := 7;
   PLLSAI_DIVR : constant := 2;

   DSI_LCD_Color_Mode : constant STM32.DSI.DSI_Color_Mode := STM32.DSI.RGB888;

   procedure Pre_LTDC_Initialize;

   procedure Post_LTDC_Initialize (Orientation : LCD_Orientation);

   procedure Default_Postinit;

end STM32.LCDInit;
