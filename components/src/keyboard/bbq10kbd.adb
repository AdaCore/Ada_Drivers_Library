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

with HAL; use HAL;
with HAL.I2C; use HAL.I2C;

package body BBQ10KBD is

   ----------
   -- Read --
   ----------

   procedure Read (This : in out BBQ10KBD_Device;
                   Reg  :        HAL.UInt8;
                   Data :    out HAL.I2C.I2C_Data)
   is
      Status : I2C_Status;
   begin
      This.Port.Mem_Read (Device_Addr,
                          UInt16 (Reg),
                          Memory_Size_8b,
                          Data,
                          Status);
      if Status /= Ok then
         raise Program_Error;
      end if;
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write (This : in out BBQ10KBD_Device;
                    Reg  :        HAL.UInt8;
                    Data :        HAL.I2C.I2C_Data)
   is
      Status : I2C_Status;
   begin
      This.Port.Mem_Write (Device_Addr,
                           UInt16 (Reg),
                           Memory_Size_8b,
                           Data,
                           Status);
      if Status /= Ok then
         raise Program_Error;
      end if;
   end Write;

   -------------
   -- Version --
   -------------

   function Version (This : in out BBQ10KBD_Device) return HAL.UInt8 is
      Data : I2C_Data (0 .. 0);
   begin
      This.Read (REG_VER, Data);
      return Data (Data'First);
   end Version;

   ------------------
   -- Key_FIFO_Pop --
   ------------------

   function Key_FIFO_Pop (This : in out BBQ10KBD_Device) return Key_State is
      Data : I2C_Data (0 .. 1);
   begin
      This.Read (REG_FIF, Data);

      return State : Key_State do
         case Data (Data'First) is
            when 1      => State.Kind := Pressed;
            when 2      => State.Kind := Held_Pressed;
            when 3      => State.Kind := Released;
            when others => State.Kind := Error;
         end case;
         State.Code := Data (Data'Last);
      end return;
   end Key_FIFO_Pop;

   ------------
   -- Status --
   ------------

   function Status (This : in out BBQ10KBD_Device) return KBD_Status is
      Data : I2C_Data (0 .. 0);
      D : UInt8 renames Data (Data'First);
   begin
      This.Read (REG_KEY, Data);

      return Stat : KBD_Status do
         Stat.Numlock := (D and 1) /= 0;
         Stat.Capslock := (D and 1) /= 0;
         Stat.Key_Count := UInt5 (D and 16#1F#);
      end return;
   end Status;

   -------------------
   -- Set_Backlight --
   -------------------

   procedure Set_Backlight (This : in out BBQ10KBD_Device; Lvl : HAL.UInt8) is
   begin
      This.Write (REG_BKL, (0 => Lvl));
   end Set_Backlight;

end BBQ10KBD;
