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

with STM32.LTDC;

package Screen_Interface is

   subtype Width  is STM32.LTDC.Width;
   subtype Height is STM32.LTDC.Height;

   type Touch_State is record
      Touch_Detected : Boolean;
      X : Width;
      Y : Height;
   end record;

   type Point is record
      X : Width;
      Y : Height;
   end record;

   function "+" (P1, P2 : Point) return Point is (P1.X + P2.X, P1.Y + P2.Y);
   function "-" (P1, P2 : Point) return Point is (P1.X - P2.X, P1.Y - P2.Y);

   subtype Color is STM32.LTDC.Pixel;

   Black      : Color renames STM32.LTDC.Black;
   White      : Color renames STM32.LTDC.White;
   Red        : Color renames STM32.LTDC.Red;
   Green      : Color renames STM32.LTDC.Green;
   Blue       : Color renames STM32.LTDC.Blue;
   Gray       : Color renames STM32.LTDC.Gray;
   Light_Gray : Color renames STM32.LTDC.Light_Gray;
   Sky_Blue   : Color renames STM32.LTDC.Sky_Blue;
   Yellow     : Color renames STM32.LTDC.Yellow;
   Orange     : Color renames STM32.LTDC.Orange;
   Pink       : Color renames STM32.LTDC.Pink;
   Violet     : Color renames STM32.LTDC.Violet;

   procedure Initialize;

   procedure Swap_Buffers;

   function Current_Touch_State return Touch_State;

   procedure Set_Pixel (P : Point; Col : Color);

   procedure Set_Pixel (X, Y : Natural; Col : Color);
   --  Same as the above Set_Pixel, just lower level formals for location

   procedure Fill_Screen (Col : Color);

   type RGB_Value is new Natural range 0 .. 255;

   function As_Color (R, G, B : RGB_Value) return Color;

end Screen_Interface;
