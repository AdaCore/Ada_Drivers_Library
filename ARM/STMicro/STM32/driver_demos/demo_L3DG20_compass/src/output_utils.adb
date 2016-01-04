------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2016, AdaCore                           --
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
--     3. Neither the name of STMicroelectronics nor the names of its       --
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

with STM32F429_Discovery;  use STM32F429_Discovery;

with STM32F4.GPIO;   use STM32F4.GPIO;
with STM32F4.RCC;    use STM32F4.RCC;
with STM32F4.SYSCFG; use STM32F4.SYSCFG;
with STM32F4.EXTI;   use STM32F4.EXTI;

package body Output_Utils is

   -----------
   -- Print --
   -----------

   procedure Print (Location : LCD_Drawing.Display_Point;  Msg : String) is
      --  a convenience routine for writing to the LCD
   begin
      LCD_Drawing.Draw_String
        (Location,
         Msg,
         Selected_Font,
         Foreground => LCD.White,  -- arbitrary
         Background => LCD.Black); -- arbitrary
   end Print;

   --------------------------
   -- Print_Static_Content --
   --------------------------

   procedure Print_Static_Content
     (Stable : Angle_Rates;
      Gain   : Float)
   is
   begin
      Print ((0, Stable_Z_Line), "Stable Z:" & Stable.Z'Img);
      Print ((0, Gain_Line), "Gain:" & Gain'Img);
      Print ((0, Heading_Line), "Heading:");
   end Print_Static_Content;

   ------------------------
   -- Initialize_Display --
   ------------------------

   procedure Initialize_Display is
   begin
      LCD.Initialize
        (Chip_Select             => (GPIO_C'Access, Pin_2),
         Enable_CS_GPIO_Clock    => GPIOC_Clock_Enable'Access,
         WRX                     => (GPIO_D'Access, Pin_13),
         Enable_WRX_GPIO_Clock   => GPIOD_Clock_Enable'Access,
         Reset                   => (GPIO_D'Access, Pin_12),
         Enable_Reset_GPIO_Clock => GPIOD_Clock_Enable'Access,
         SPI_Chip                => SPI_5'Access,
         Enable_SPI_Clock        => SPI5_Clock_Enable'Access,
         SPI_GPIO                => GPIO_F'Access,
         Enable_SPI_GPIO_Clock   => GPIOF_Clock_Enable'Access,
         SPI_AF                  => GPIO_AF_SPI5,
         SCK_Pin                 => Pin_7,
         MISO_Pin                => Pin_8,
         MOSI_Pin                => Pin_9);

      LCD.Set_Orientation (To => LCD.Portrait_2);

      LCD.Fill (LCD.Black);
   end Initialize_Display;

end Output_Utils;
