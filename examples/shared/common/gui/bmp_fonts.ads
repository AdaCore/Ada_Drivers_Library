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
--                                                                          --
--  This file is based on:                                                  --
--                                                                          --
--   @file    fonts.h                                                       --
--   @author  MCD Application Team                                          --
--   @version V1.1.0                                                        --
--   @date    19-June-2014                                                  --
--   @brief   Header for fonts.c file.                                      --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

with HAL; use HAL;

package BMP_Fonts is

   type BMP_Font is (Font8x8, Font12x12, Font16x24);

   function Data
     (Font          : BMP_Font;
      C             : Character;
      Height_Offset : Natural)
      return UInt16 with Inline;
   --  Provides the numeric data values representing individual characters. For
   --  the given font and character within that font, returns the numeric value
   --  representing the bits at the Height_Offset within the representation.

   function Mask (Font : BMP_Font; Width_Offset : Natural) return UInt16
     with Inline;
   --  Provides the mask value used to test individual bits in the data
   --  values representing individual characters. For example, to see if bit W
   --  within the bits representing Char at height H is set, you would do the
   --  following:
   --
   --       if (Data (Font, Char, H) and Mask (Font, W)) /= 0 then

   function Char_Height (Font : BMP_Font) return Natural with Inline;
   --  Returns the height of any individual character in the specified font, in
   --  terms of pixels .

   function Char_Width (Font : BMP_Font) return Natural with Inline;
   --  Returns the width of any individual character in the specified font, in
   --  terms of pixels .

end BMP_Fonts;
