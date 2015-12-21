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

with STM32.LCD;

package Screen_Interface is

   subtype Width  is STM32.LCD.Width;
   subtype Height is STM32.LCD.Height;

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

   subtype Color is STM32.Half_Word;

   Black      : Color := 2#1_00000_00000_00000#;
   White      : Color := 2#1_11111_11111_11111#;
   Red        : Color := 2#1_11111_00000_00000#;
   Green      : Color := 2#1_00000_11111_00000#;
   Blue       : Color := 2#1_00000_00000_11111#;
   Gray       : Color := 2#1_10111_10111_10111#;
   Light_Gray : Color := 2#1_11100_11100_11100#;
   Sky_Blue   : Color := 2#1_10011_11010_11111#;
   Yellow     : Color := 2#1_11111_11111_00000#;
   Orange     : Color := 2#1_11111_10101_00000#;
   Pink       : Color := 2#1_11111_01101_10111#;
   Violet     : Color := 2#1_10011_00110_11010#;

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
