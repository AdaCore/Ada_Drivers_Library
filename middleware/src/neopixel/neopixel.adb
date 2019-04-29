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

package body NeoPixel is

   Stride : constant array (LED_Mode) of Positive :=
     (RGBW => 4, others => 3);

   ------------
   -- Create --
   ------------

   function Create (Mode : LED_Mode; Count : Positive) return LED_Strip is
   begin
      return LED_Strip'(Mode     => Mode,
                        Count    => Count,
                        Buf_Last => Count * Stride (Mode) - 1,
                        Buffer   => (others => 0));
   end Create;

   type Component_Indices is array (LED_Component) of Integer;
   Mode_Indices : constant array (LED_Mode) of Component_Indices :=
     (RGB  => (LED_Red => 0, LED_Green => 1, LED_Blue => 2, LED_White => -1),
      GRB  => (LED_Red => 1, LED_Green => 0, LED_Blue => 2, LED_White => -1),
      RGBW => (LED_Red => 0, LED_Green => 1, LED_Blue => 2, LED_White => 3));

   ---------------
   -- Set_Color --
   ---------------

   procedure Set_Color
     (Strip : in out LED_Strip;
      Index : Natural;
      Color : LED_Values)
   is
      pragma Assert (Index < Strip.Count);
      Base    : constant Natural := Index * Stride (Strip.Mode);
      Indices : constant Component_Indices := Mode_Indices (Strip.Mode);
   begin
      for J in LED_Red .. (if Strip.Mode = RGBW then LED_White else LED_Blue) loop
         Strip.Buffer (Base + Indices (J)) := Color (J);
      end loop;
   end Set_Color;

   ----------
   -- Show --
   ----------

   procedure Show
     (Strip : LED_Strip;
      Write : access procedure (Buffer : UInt8_Array))
   is
   begin
      Write (Strip.Buffer);
   end Show;

end NeoPixel;
