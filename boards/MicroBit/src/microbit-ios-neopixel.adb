------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2019-2020, AdaCore                      --
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

--  Bit-bang a sequence of color LED values to a one-pin LED strip (WS2812B
--  or similar).

with System.Machine_Code; use System, System.Machine_Code;

with NRF_SVD.GPIO; use NRF_SVD.GPIO;
with MicroBit.Time;

package body MicroBit.IOs.NeoPixel is

   -----------
   -- Write --
   -----------

   procedure Write (Pin : Pin_Id; Values : UInt8_Array) is
      Point : constant GPIO_Point := Points (Pin);
      Mask  : constant UInt32 := 2 ** Point.Pin;
      EOL   : constant String := ASCII.LF & ASCII.HT;
   begin
      --  Set pin to digital out mode and reset strip

      Set (Pin, False);
      MicroBit.Time.Delay_Ms (1);

      --  The following loop is very time-sensitive. The bit frame is 1.25 µs,
      --  i.e. 20 cycles of the 16 MHz clock. So, we run it with IRQs disabled,
      --  and carefully account for every cycle.
      --  Code taken from:
      --  https://github.com/Microsoft/pxt-ws2812b/blob/master/sendBuffer.asm

      Asm ("    cpsid i             @  disable irq"    & EOL &
           "    b .start"                              & EOL &

           ".nextbit:               @             c0"  & EOL &
           "    str %2, [%1, #0]    @  pin := hi  c2"  & EOL &
           "    tst r6, r0          @             c3"  & EOL &
           "    bne .islate         @             c4"  & EOL &
           "    str %2, [%0, #0]    @  pin := lo  c6"  & EOL &

           ".islate:"                                  & EOL &
           "    lsr r6, r6, #1      @  r6 >>= 1   c7"  & EOL &
           "    bne .justbit        @             c8"  & EOL &

           "@ not just a bit - need new byte"          & EOL &

           "    add %3, #1          @ buf++       c9"  & EOL &
           "    sub %4, #1          @ len--       c10" & EOL &
           "    bcc .stop           @ if (len<0) goto .stop c11" & EOL &

           ".start:"                                   & EOL &
           "    movs r6, #0x80      @ reset mask  c12" & EOL &
           "    nop                 @             c13" & EOL &

           ".common:                @             c13" & EOL &
           "    str %2, [%0, #0]    @ pin := lo   c15" & EOL &

           "    @ always re-load byte - it just fits with the" & EOL &
           "    @ cycles better this way."                     & EOL &

           "    ldrb r0, [%3, #0]   @ r0 := *buf  c17" & EOL &
           "    b .nextbit          @             c20" & EOL &

           ".justbit:               @             c10" & EOL &

           "    @ no nops, branch taken is already 3 cycles" & EOL &
           "    b .common           @             c13" & EOL &

           ".stop:"                                    & EOL &
           "    str %2, [%0, #0]    @ pin := lo"       & EOL &
           "    cpsie i             @ enable irq",

           Inputs => (Address'Asm_Input ("l", GPIO_Periph.OUTCLR'Address), -- %0
                      Address'Asm_Input ("l", GPIO_Periph.OUTSET'Address), -- %1
                      UInt32'Asm_Input  ("l", Mask),                       -- %2
                      Address'Asm_Input ("l", Values'Address),             -- %3
                      Natural'Asm_Input ("l", Values'Length)),             -- %4
           Volatile => True,
           Clobber => "r0,r6");
   end Write;

end MicroBit.IOs.NeoPixel;
