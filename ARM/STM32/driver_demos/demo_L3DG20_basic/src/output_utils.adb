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

--  with STM32F429_Discovery;  use STM32F429_Discovery;
--
--  with STM32F4.GPIO;   use STM32F4.GPIO;
--  with STM32F4.RCC;    use STM32F4.RCC;
--  with STM32F4.SYSCFG; use STM32F4.SYSCFG;
--  with STM32F4.EXTI;   use STM32F4.EXTI;

package body Output_Utils is

   -----------
   -- Print --
   -----------

   procedure Print (Location : LCD_Drawing.Display_Point;  Msg : String) is
      --  a convenience routine for writing to the LCD
   begin
      LCD_Drawing.Draw_String
        (LCD_Drawing.Screen_Buffer,
         Location,
         Msg,
         Selected_Font,
         Foreground => LCD_Drawing.White,  -- arbitrary
         Background => LCD_Drawing.Black); -- arbitrary
   end Print;

   --------------------------
   -- Print_Static_Content --
   --------------------------

   procedure Print_Static_Content (Stable : Angle_Rates) is
   begin
      --  print the constant offsets computed when the device is motionless
      Print ((0, Line1_Stable), "Stable X:" & Stable.X'Img);
      Print ((0, Line2_Stable), "Stable Y:" & Stable.Y'Img);
      Print ((0, Line3_Stable), "Stable Z:" & Stable.Z'Img);

      --  print the static labels for the values before the offset is removed
      Print ((0, Line1_Raw), "Raw X:");
      Print ((0, Line2_Raw), "Raw Y:");
      Print ((0, Line3_Raw), "Raw Z:");

      --  print the static labels for the values after the offset is removed
      Print ((0, Line1_Adjusted), "Adjusted X:");
      Print ((0, Line2_Adjusted), "Adjusted Y:");
      Print ((0, Line3_Adjusted), "Adjusted Z:");

      --  print the static labels for the final values
      Print ((0, Line1_Final), "X:");
      Print ((0, Line2_Final), "Y:");
      Print ((0, Line3_Final), "Z:");
  end Print_Static_Content;

   ------------------------
   -- Initialize_Display --
   ------------------------

   procedure Initialize_Display is
   begin
      STM32.LCD.Initialize (STM32.LCD.Pixel_Fmt_ARGB1555);
      STM32.DMA2D.Polling.Initialize;

      STM32.LCD.Set_Orientation (STM32.LCD.Portrait);

      STM32.DMA2D.DMA2D_Fill (LCD_Drawing.Screen_Buffer, 0);
   end Initialize_Display;

end Output_Utils;
