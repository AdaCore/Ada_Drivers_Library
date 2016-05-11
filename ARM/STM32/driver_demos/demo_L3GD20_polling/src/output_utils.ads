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

with L3GD20;    use L3GD20;
with BMP_Fonts; use BMP_Fonts;

package Output_Utils is

   procedure Print_Static_Content (Stable : Angle_Rates);

   procedure Print (X, Y : Natural;  Msg : String);

   --  these constants are used for displaying values on the LCD

   Selected_Font : constant BMP_Font := Font12x12;
   Line_Height   : constant Positive := Char_Height (Selected_Font) + 4;

   --  the locations on the screen for the stable offsets
   Line1_Stable : constant Natural := 0;
   Line2_Stable : constant Natural := Line1_Stable + Line_Height;
   Line3_Stable : constant Natural := Line2_Stable + Line_Height;

   --  the locations on the screen for the raw values
   Line1_Raw : constant Natural := 55; -- leaves room for printing stable values
   Line2_Raw : constant Natural := Line1_Raw + Line_Height;
   Line3_Raw : constant Natural := Line2_Raw + Line_Height;

   --  the column number for displaying raw values dynamically, based on
   --  the length of the longest static label
   Col_Raw : constant Natural := String'("Raw X:")'Length * Char_Width (Selected_Font);

   --  the locations on the screen for values after the offset is removed
   Line1_Adjusted : constant Natural := 110; -- leaves room for printing stable values
   Line2_Adjusted : constant Natural := Line1_Adjusted + Line_Height;
   Line3_Adjusted : constant Natural := Line2_Adjusted + Line_Height;

   --  the column number for displaying adjusted values dynamically, based on
   --  the length of the longest static label
   Col_Adjusted : constant Natural := String'("Adjusted X:")'Length * Char_Width (Selected_Font);

   --  the locations on the screen for the final scaled values
   Line1_Final : constant Natural := 165; -- leaves room for printing adjusted values
   Line2_Final : constant Natural := Line1_Final + Line_Height;
   Line3_Final : constant Natural := Line2_Final + Line_Height;

   --  the column number for displaying the final values dynamically, based on
   --  the length of the longest static label
   Final_Column : constant Natural := String'("X:")'Length * Char_Width (Selected_Font);

end Output_Utils;
