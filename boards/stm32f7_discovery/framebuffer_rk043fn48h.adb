with STM32.Device;         use STM32.Device;
with STM32.GPIO;           use STM32.GPIO;

package body Framebuffer_RK043FN48H is

   LCD_BL_CTRL   : GPIO_Point renames PK3;
   LCD_ENABLE    : GPIO_Point renames PI12;

   LCD_HSYNC     : GPIO_Point renames PI10;
   LCD_VSYNC     : GPIO_Point renames PI9;
   LCD_CLK       : GPIO_Point renames PI14;
   LCD_DE        : GPIO_Point renames PK7;
   LCD_INT       : GPIO_Point renames PI13;

   NC1           : GPIO_Point renames PI8;

   LCD_CTRL_PINS : constant GPIO_Points :=
                     (LCD_VSYNC, LCD_HSYNC, LCD_INT,
                      LCD_CLK, LCD_DE, NC1);
   LCD_RGB_AF14  : constant GPIO_Points :=
                     (PI15, PJ0, PJ1, PJ2, PJ3, PJ4, PJ5, PJ6, --  Red
                      PJ7, PJ8, PJ9, PJ10, PJ11, PK0, PK1, PK2, --  Green
                      PE4, PJ13, PJ14, PJ15, PK4, PK5, PK6); --  Blue
   LCD_RGB_AF9   : constant GPIO_Points :=
                     (1 => PG12); -- B4

   procedure Init_Pins;

   ---------------
   -- Init_Pins --
   ---------------

   procedure Init_Pins
   is
      LTDC_Pins : constant GPIO_Points :=
                    LCD_CTRL_PINS & LCD_RGB_AF14 & LCD_RGB_AF9;

   begin
      Enable_Clock (LTDC_Pins);

      Configure_Alternate_Function
        (LCD_CTRL_PINS & LCD_RGB_AF14, GPIO_AF_LTDC);
      Configure_Alternate_Function (LCD_RGB_AF9, GPIO_AF_LTDC_2);
      Configure_IO
        (Points => LTDC_Pins,
         Config => (Speed       => Speed_50MHz,
                    Mode        => Mode_AF,
                    Output_Type => Push_Pull,
                    Resistors   => Floating));
      Lock (LTDC_Pins);

      Configure_IO
        (GPIO_Points'(LCD_ENABLE, LCD_BL_CTRL),
         Config => (Speed       => Speed_2MHz,
                    Mode        => Mode_Out,
                    Output_Type => Push_Pull,
                    Resistors   => Pull_Down));
      Lock (LCD_ENABLE & LCD_BL_CTRL);
   end Init_Pins;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Display     : in out Frame_Buffer;
      Orientation : HAL.Framebuffer.Display_Orientation := Default;
      Mode        : HAL.Framebuffer.Wait_Mode := Interrupt)
   is
   begin
      Init_Pins;
      Display.Initialize
        (Width         => LCD_Natural_Width,
         Height        => LCD_Natural_Height,
         H_Sync        => 41,
         H_Back_Porch  => 13,
         H_Front_Porch => 32,
         V_Sync        => 10,
         V_Back_Porch  => 2,
         V_Front_Porch => 2,
         PLLSAI_N      => 192,
         PLLSAI_R      => 5,
         DivR          => 4,
         Orientation   => Orientation,
         Mode          => Mode);

      STM32.GPIO.Set (LCD_ENABLE);
      STM32.GPIO.Set (LCD_BL_CTRL);
   end Initialize;

end Framebuffer_RK043FN48H;
