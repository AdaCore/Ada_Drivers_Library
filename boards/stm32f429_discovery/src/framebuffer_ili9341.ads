with HAL;             use HAL;
with HAL.Framebuffer; use HAL.Framebuffer;

with Framebuffer_LTDC;
private with ILI9341;
private with STM32.GPIO;
private with STM32.Device;

package Framebuffer_ILI9341 is

   type Frame_Buffer is limited
     new Framebuffer_LTDC.Frame_Buffer with private;

   procedure Initialize
     (Display     : in out Frame_Buffer;
      Orientation : HAL.Framebuffer.Display_Orientation := Default;
      Mode        : HAL.Framebuffer.Wait_Mode := Interrupt);

private

   --  Chip select and Data/Command select for the LCD screen
   LCD_CSX      : STM32.GPIO.GPIO_Point renames STM32.Device.PC2;
   LCD_WRX_DCX  : STM32.GPIO.GPIO_Point renames STM32.Device.PD13;
   LCD_RESET    : STM32.GPIO.GPIO_Point renames STM32.Device.PD12;

   type Frame_Buffer
   is limited new Framebuffer_LTDC.Frame_Buffer with record
      Device : ILI9341.ILI9341_Device (STM32.Device.SPI_5'Access,
                                       Chip_Select => LCD_CSX'Access,
                                       WRX         => LCD_WRX_DCX'Access,
                                       Reset       => LCD_RESET'Access);
   end record;

end Framebuffer_ILI9341;
