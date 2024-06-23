------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2015-2024, AdaCore                     --
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

with HAL.Framebuffer;

with STM32.Device;

with ILI9341.Device.Bitmap;
with ILI9341.Device;
with ILI9341.SPI_Connector;

package Display_ILI9341 is

   type Display is tagged limited private;

   procedure Initialize (This : in out Display);
   --  Configure FSMC, turn backlight on and initialize the display

   procedure Set_Orientation
     (This        : in out Display;
      Orientation : HAL.Framebuffer.Display_Orientation);

   package ILI9341_Device is new ILI9341.Device
     (ILI9341_Connector => ILI9341.SPI_Connector.ILI9341_Connector,
      Send_Command      => ILI9341.SPI_Connector.Send_Command,
      Connection        => ILI9341.Parallel,
      Connector         =>
        (Port        => STM32.Device.SPI_1'Access,
         Chip_Select => STM32.Device.PA4'Access,
         WRX         => STM32.Device.PA2'Access));

   package ILI9341_Bitmap is new ILI9341_Device.Bitmap
     (ILI9341.SPI_Connector.Write_Pixels,
      ILI9341.SPI_Connector.Read_Pixels);

   subtype Bitmap_Buffer is ILI9341_Bitmap.Bitmap_Buffer;

   function Buffer (This : in out Display) return Bitmap_Buffer;

private

   type Display is tagged limited record
      Device : aliased ILI9341_Device.ILI9341_Device;
   end record;

   function Buffer (This : in out Display) return Bitmap_Buffer is
     (ILI9341_Bitmap.Get_Bitmap (This.Device'Access));

end Display_ILI9341;
