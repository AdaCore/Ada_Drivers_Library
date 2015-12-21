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

--  This program demonstrates some basic functionality of the ILI9341 LCD
--  component driver, combined with the Bitmapped_Drawing generic package.

with STM32.GPIO;         use STM32.GPIO;

with Bitmapped_Drawing;
with STM32.ILI9341;
with BMP_Fonts;          use BMP_Fonts;

with STM32.Device;       use STM32.Device;

procedure Demo_ILI9341 is

   package LCD renames STM32.ILI9341; use LCD;

   package LCD_Drawing is new Bitmapped_Drawing
     (Color     => LCD.Colors,
      Set_Pixel => LCD.Set_Pixel);

   use LCD_Drawing;

begin
   LCD.Initialize
     (Chip_Select             => PC2,
      WRX                     => PD13,
      Reset                   => PD12,
      SPI_Chip                => SPI_5'Access,
      SPI_AF                  => GPIO_AF_SPI5,
      SCK_Pin                 => PF7,
      MISO_Pin                => PF8,
      MOSI_Pin                => PF9);

   LCD.Set_Orientation (To => LCD.Portrait_2);

   LCD.Fill (Color => LCD.Orange);

   declare
      Chars : constant String := "abcdefghijkl";
   begin
      Draw_String
        (Start      => (0, 0),
         Msg        => Chars,
         Font       => Font16x24,
         Foreground => Black,
         Background => Orange);
   end;

   Draw_Circle
     (Center => (40, 220),
      Radius => 20,
      Hue    => Blue,
      Fill   => True);

   loop
      null;
   end loop;
end Demo_ILI9341;
