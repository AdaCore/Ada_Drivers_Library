------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2022, AdaCore                      --
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

with HAL;        use HAL;
with HAL.I2C;    use HAL.I2C;

--  I2C 8-bit IO expander with quasi bidirectional I/O, no data
--  direction, no latch

package PCF8574 is

   subtype PCF8574_Address is I2C_Address range 16#40# .. 16#5F#;

   type PCF8574_Module (Port : not null Any_I2C_Port;
                        Addr : I2C_Address) is tagged limited private;

   type Any_PCF8574_Module is access all PCF8574_Module'Class;


   procedure Set (This : PCF8574_Module; Data : UInt8);

   function Get (This : PCF8574_Module) return UInt8;
   procedure Get (This : PCF8574_Module; Data : out UInt8);
   --  when reading the input from keys (buttons) carefully read the
   --  datasheet. The input line should be set high before reading.
   --  E.g. if all lines are key input:
   --  M.Set (16#FF#);
   --  Keys := M.Get;

private

   type PCF8574_Module (Port : not null Any_I2C_Port;
                        Addr : I2C_Address) is tagged limited null record;

end PCF8574;
