------------------------------------------------------------------------------
--                                                                          --
--                        Copyright (C) 2020, AdaCore                       --
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

--  Driver for the Blackberry Q10 Keyboard I2C interface from solder.party
--  https://github.com/arturo182/bbq10kbd_i2c_sw

with HAL;
with HAL.I2C;

package BBQ10KBD is

   type BBQ10KBD_Device (Port : not null HAL.I2C.Any_I2C_Port)
   is tagged private;

   type Key_State_Kind is (Pressed, Held_Pressed, Released, Error);

   type Key_State is record
      Kind : Key_State_Kind;
      Code : HAL.UInt8;
   end record;

   function Key_FIFO_Pop (This : in out BBQ10KBD_Device) return Key_State;
   --  When the FIFO is empty a Key_State with Kind = Error is returned

   type KBD_Status is record
      Numlock   : Boolean;
      Capslock  : Boolean;
      Key_Count : HAL.UInt5;
   end record;

   function Status (This : in out BBQ10KBD_Device) return KBD_Status;

   procedure Set_Backlight (This : in out BBQ10KBD_Device;
                            Lvl  :        HAL.UInt8);

   function Version (This : in out BBQ10KBD_Device) return HAL.UInt8;

private

   type BBQ10KBD_Device (Port : not null HAL.I2C.Any_I2C_Port)
   is tagged null record;

   procedure Read (This : in out BBQ10KBD_Device;
                   Reg  :        HAL.UInt8;
                   Data :    out HAL.I2C.I2C_Data);

   procedure Write (This : in out BBQ10KBD_Device;
                    Reg  :        HAL.UInt8;
                    Data :        HAL.I2C.I2C_Data);

   Device_Addr : constant HAL.I2C.I2C_Address := 16#1F#;

   REG_VER : constant := 16#01#;
   REG_CFG : constant := 16#02#;
   REG_INT : constant := 16#03#;
   REG_KEY : constant := 16#04#;
   REG_BKL : constant := 16#05#;
   REG_DEB : constant := 16#06#;
   REG_FRQ : constant := 16#07#;
   REG_RST : constant := 16#08#;
   REG_FIF : constant := 16#09#;

end BBQ10KBD;
