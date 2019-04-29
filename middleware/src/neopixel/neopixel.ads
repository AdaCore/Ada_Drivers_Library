------------------------------------------------------------------------------
--                                                                          --
--                        Copyright (C) 2019, AdaCore                       --
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

with HAL; use HAL;

package NeoPixel is

   type LED_Component is (LED_Red, LED_Green, LED_Blue, LED_White);
   subtype LED_Value is UInt8;
   type LED_Mode is
     (GRB,   --  Most NeoPixel products (WS2812)
      RGB,   --  FLORA v1 (not v2) pixels
      RGBW); --  NeoPixel RGB+white products

   type LED_Values is array (LED_Component) of LED_Value;

   Red    : constant LED_Values := (LED_Red => 255, others => 0);
   Orange : constant LED_Values := (LED_Red => 255, LED_Green => 165, others => 0);
   Yellow : constant LED_Values := (LED_Red | LED_Green => 255, others => 0);
   Green  : constant LED_Values := (LED_Green => 255, others => 0);
   Blue   : constant LED_Values := (LED_Blue  => 255, others => 0);
   Indigo : constant LED_Values := (LED_Red => 75, LED_Blue => 130, others => 0);
   Purple : constant LED_Values := (LED_Red | LED_Blue => 255, others => 0);
   White  : constant LED_Values := (others => 255);
   Black  : constant LED_Values := (others => 0);

   type LED_Strip (<>) is private;

   function Create (Mode : LED_Mode; Count : Positive) return LED_Strip;

   procedure Set_Color
     (Strip : in out LED_Strip;
      Index : Natural;
      Color : LED_Values);
   --  Set the color of the designated pixel to the given color

   procedure Show
     (Strip : LED_Strip;
      Write : access procedure (Buffer : UInt8_Array));

private

   type LED_Strip (Mode : LED_Mode; Count : Positive; Buf_Last : Positive) is record
      Buffer : aliased UInt8_Array (0 .. Buf_Last);
   end record;

end NeoPixel;
