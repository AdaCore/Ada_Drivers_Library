------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2022, AdaCore                          --
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

with HAL.I2C; use HAL.I2C;

package body MCP23x17.I2C is

   --------------
   -- IO_Write --
   --------------

   overriding
   procedure IO_Write
     (This  : in out MCP23017_IO_Expander;
      Addr  : Register_Address;
      Value : UInt8)
   is
      Status : I2C_Status;
   begin
      This.Port.Mem_Write
        (Base_Address or I2C_Address (This.Addr),
         UInt16 (Addr),
         Memory_Size_8b,
         (1 => Value),
         Status,
         1000);

      if Status /= Ok then
         --  No error handling...
         raise Program_Error;
      end if;
   end IO_Write;

   -------------
   -- IO_Read --
   -------------

   overriding
   procedure IO_Read
     (This  : MCP23017_IO_Expander;
      Addr  : Register_Address;
      Value : out UInt8)
   is
      Ret    : I2C_Data (1 .. 1);
      Status : I2C_Status;
   begin
      This.Port.Mem_Read
        (Base_Address or I2C_Address (This.Addr),
         UInt16 (Addr),
         Memory_Size_8b,
         Ret,
         Status,
         1000);

      if Status /= Ok then
         --  No error handling...
         raise Program_Error;
      end if;

      Value := Ret (1);
   end IO_Read;

end MCP23x17.I2C;
