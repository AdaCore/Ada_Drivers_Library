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

      Configure_IO
        (Points => LTDC_Pins,
         Config => (Mode           => Mode_AF,
                    AF             => GPIO_AF_LTDC_14,
                    AF_Speed       => Speed_50MHz,
                    AF_Output_Type => Push_Pull,
                    Resistors      => Floating));

      Configure_Alternate_Function (LCD_RGB_AF9, GPIO_AF_LTDC_9);

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
