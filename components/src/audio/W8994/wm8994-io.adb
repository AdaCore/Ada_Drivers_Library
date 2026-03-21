------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2026, AdaCore                          --
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

package body WM8994.IO is

   ---------------
   -- I2C_Write --
   ---------------

   procedure I2C_Write
     (This     : in out Audio_CODEC;
      Register : Register_Address;
      Value    : UInt16)
   is
      Status : I2C_Status;
      Data   : I2C_Data (1 .. 2);
      Check  : UInt16 with Unreferenced;
   begin
      --  Device is MSB first
      Data (1) := UInt8 (Shift_Right (Value and 16#FF00#, 8));
      Data (2) := UInt8 (Value and 16#FF#);

      This.Port.Mem_Write
        (Addr          => This.I2C_Addr,
         Mem_Addr      => UInt16 (Register),
         Mem_Addr_Size => Memory_Size_16b,
         Data          => Data,
         Status        => Status);

      if Register /= 0 then
         Check := I2C_Read (This, Register);
      end if;
   end I2C_Write;

   --------------
   -- I2C_Read --
   --------------

   function I2C_Read
     (This     : in out Audio_CODEC;
      Register : Register_Address)
   return UInt16
   is
      Status : I2C_Status;
      Data   : I2C_Data (1 .. 2);
      Result : UInt16;
   begin
      This.Port.Mem_Read
        (Addr          => This.I2C_Addr,
         Mem_Addr      => UInt16 (Register),
         Mem_Addr_Size => Memory_Size_16b,
         Data          => Data,
         Status        => Status);
      Result := Shift_Left (UInt16 (Data (1)), 8) or UInt16 (Data (2));
      return Result;
   end I2C_Read;

end WM8994.IO;
