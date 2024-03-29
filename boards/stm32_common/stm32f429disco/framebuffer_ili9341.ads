------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2015-2016, AdaCore                     --
--                                                                          --
--  Redistribution and use in source and binary forms, with or without      --
--  modification, are permitted provided that the following conditions are  --
--  met:                                                                    --
--     1. Redistributions of source code must retain the above copyright    --
--        notice, this list of conditions and the following disclaimer.     --
--     2. Redistributions in binary form must reproduce the above copyright --
--        notice, this list of conditions and the following disclaimer in   --
--        the documentation and/or other materials provided with the        --
--        distribution.                                                     --
--     3. Neither the name of the copyright holder nor the names of its     --
--        contributors may be used to endorse or promote products derived   --
--        from this software without specific prior written permission.     --
--                                                                          --
--   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS    --
--   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT      --
--   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR  --
--   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT   --
--   HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, --
--   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT       --
--   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,  --
--   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY  --
--   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT    --
--   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE  --
--   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.   --
--                                                                          --
------------------------------------------------------------------------------

with HAL;             use HAL;
with HAL.Framebuffer; use HAL.Framebuffer;

with Framebuffer_LTDC;
private with ILI9341.Device;
private with ILI9341.SPI_Connector;

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

   package RGB_SPI_Device is new ILI9341.Device
     (ILI9341_Connector => ILI9341.SPI_Connector.ILI9341_Connector,
      Send_Command      => ILI9341.SPI_Connector.Send_Command,
      Connection        => ILI9341.RGB,
      Connector         =>
        (Port        => STM32.Device.SPI_5'Access,
         Chip_Select => LCD_CSX'Access,
         WRX         => LCD_WRX_DCX'Access));

   type Frame_Buffer is limited new Framebuffer_LTDC.Frame_Buffer with record
      Device : RGB_SPI_Device.ILI9341_Device;
   end record;

end Framebuffer_ILI9341;
