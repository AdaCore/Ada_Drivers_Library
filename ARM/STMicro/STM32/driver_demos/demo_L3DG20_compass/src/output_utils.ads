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

with Bitmapped_Drawing;
with BMP_Fonts;           use BMP_Fonts;
with STM32.LCD;           use STM32.LCD;
with STM32.DMA2D.Polling; use STM32.DMA2D;

with STM32.L3DG20;        use STM32.L3DG20;

package Output_Utils is

   package LCD_Drawing renames Bitmapped_Drawing;

   procedure Print_Static_Content
     (Stable : Angle_Rates;
      Gain   : Float);

   procedure Print (Location : LCD_Drawing.Display_Point;  Msg : String);

   procedure Initialize_Display;
   --  call this first

   --  these constants are used for displaying values on the LCD

   Selected_Font : constant BMP_Font := Font12x12;

   Stable_Z_Line : constant Natural := 0;    --  arbitrary
   Gain_Line     : constant Natural := 20;   --  arbitrary
   Heading_Line  : constant Natural := 60;   --  arbitrary

   --  the column number for displaying the final values dynamically, based on
    --  the length of the longest static label
   Heading_Column : constant Natural :=
                      String'("Heading:")'Length * Char_Width (Selected_Font);
end Output_Utils;
