------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2015-2017, AdaCore                     --
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
with HAL.Bitmap;

with Framebuffer_DSI;

private with STM32.Device;
private with STM32.DMA2D_Bitmap;
private with STM32.DSI;
private with STM32.GPIO;

package Framebuffer_OTM8009A is

   LCD_Natural_Width  : constant := Framebuffer_DSI.LCD_Natural_Width;
   LCD_Natural_Height : constant := Framebuffer_DSI.LCD_Natural_Height;

   type Frame_Buffer is limited
     new HAL.Framebuffer.Frame_Buffer_Display with private;

   procedure Initialize
     (Display     : in out Frame_Buffer;
      Orientation : HAL.Framebuffer.Display_Orientation := Default;
      Mode        : HAL.Framebuffer.Wait_Mode := Interrupt);

private

   DSI_RESET   : STM32.GPIO.GPIO_Point renames STM32.Device.PJ15;

   PLLSAIN     : constant := 417;
   PLLSAIR     : constant := 5;
   PLLSAI_DIVR : constant := 2;
   PLL_N_Div   : constant := 100;
   PLL_IN_Div  : constant STM32.DSI.DSI_PLL_IDF := STM32.DSI.PLL_IN_DIV5;
   PLL_OUT_Div : constant STM32.DSI.DSI_PLL_ODF := STM32.DSI.PLL_OUT_DIV1;

   type Frame_Buffer is limited new Framebuffer_DSI.Frame_Buffer
     with null record;

end Framebuffer_OTM8009A;
