with STM32.Board;  use STM32.Board;
with STM32.Device; use STM32.Device;
with STM32.GPIO;   use STM32.GPIO;

package body STM32.LCDInit is

   -------------------
   -- LCD_Pins_Init --
   -------------------

   procedure Initialize
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

      GPIO.Set (LCD_BL_CTRL);
      GPIO.Set (LCD_ENABLE);
   end Initialize;

end STM32.LCDInit;
