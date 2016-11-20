------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2016, AdaCore                           --
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

package body BNO055_I2C_IO is

   ----------
   -- Read --
   ----------

   procedure Read
     (This     : Any_IO_Port;
      Register : UInt8;
      Value    : out UInt8)
   is
      Incoming : I2C_Data (1 .. 1);
      Result   : I2C_Status;
   begin
      Mem_Read
        (This.Port.all,
         This.Device,
         Mem_Addr      => UInt16 (Register),
         Mem_Addr_Size => Memory_Size_8b,
         Data          => Incoming,
         Status        => Result);
      if Result /= Ok then
         raise Program_Error with "I2C read error:" & Result'Img;
      end if;
      Value := Incoming (1);
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write
     (This     : Any_IO_Port;
      Register : UInt8;
      Value    : UInt8)
   is
      Outgoing : constant I2C_Data (1 .. 1) := (1 => Value);
      Result   : I2C_Status;
   begin
      Mem_Write
        (This.Port.all,
         This.Device,
         Mem_Addr      => UInt16 (Register),
         Mem_Addr_Size => Memory_Size_8b,
         Data          => Outgoing,
         Status        => Result);
      if Result /= Ok then
         raise Program_Error with "I2C write error:" & Result'Img;
      end if;
   end Write;

   -----------------
   -- Read_Buffer --
   -----------------

   procedure Read_Buffer
     (This     : Any_IO_Port;
      Register : UInt8;
      Value    : out I2C_Data)
   is
      Result   : I2C_Status;
   begin
      Mem_Read
        (This.Port.all,
         This.Device,
         Mem_Addr      => UInt16 (Register),
         Mem_Addr_Size => Memory_Size_8b,
         Data          => Value,
         Status        => Result);
      if Result /= Ok then
         raise Program_Error with "I2C read buff error:" & Result'Img;
      end if;
   end Read_Buffer;

end BNO055_I2C_IO;
