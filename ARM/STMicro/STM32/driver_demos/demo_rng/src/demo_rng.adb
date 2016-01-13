------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2015, AdaCore                           --
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

--  Basic demonstration of on-board RNG hardware.

--  Note that you may need to cycle power on the board.

with STM32F429_Discovery;  use STM32F429_Discovery;

with STM32F4.ILI9341;
with Bitmapped_Drawing;
with LCD_Std_Out;

--  with STM32F4.RNG.Polling;  use STM32F4.RNG.Polling;
with STM32F4.RNG.Interrupts;  use STM32F4.RNG.Interrupts;

procedure Demo_RNG is

   package LCD_Drawing is new Bitmapped_Drawing
     (Color     => STM32F4.ILI9341.Colors,
      Set_Pixel => STM32F4.ILI9341.Set_Pixel);

   package LCD_Text is new LCD_Std_Out (LCD_Drawing);
   --  we use the LCD_Std_Out generic, rather than directly using the Drawing
   --  package, because we want the effect of a New_Line

   use LCD_Text;

begin
   Initialize_LCD_Hardware;
   LCD_Text.Initialize;
   Initialize_RNG;
   loop
      Put_Line (Random'Img & "  ");
   end loop;
end Demo_RNG;
