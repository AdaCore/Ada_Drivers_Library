------------------------------------------------------------------------------
--                                                                          --
--                       Copyright (C) 2017, AdaCore                        --
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

with Ada.Unchecked_Conversion;
with Interfaces; use Interfaces;

package body MMA8653 is

   function To_Int_16 is new Ada.Unchecked_Conversion (Unsigned_16, Integer_16);

   function Read_Register (This : MMA8653_Accelerometer'Class;
                           Addr : Register_Addresss) return UInt8;

   -------------------
   -- Read_Register --
   -------------------

   function Read_Register (This : MMA8653_Accelerometer'Class;
                           Addr : Register_Addresss) return UInt8
   is
      Data   : I2C_Data (1 .. 1);
      Status : I2C_Status;
   begin
      This.Port.Mem_Read (Addr          => Device_Address,
                          Mem_Addr      => UInt16 (Addr),
                          Mem_Addr_Size => Memory_Size_8b,
                          Data          => Data,
                          Status        => Status);

      if Status /= Ok then
         --  No error handling...
         raise Program_Error;
      end if;
      return Data (Data'First);
   end Read_Register;

   ---------------------
   -- Check_Device_Id --
   ---------------------

   function Check_Device_Id (This : MMA8653_Accelerometer) return Boolean is
   begin
      return Read_Register (This, Who_Am_I) = Device_Id;
   end Check_Device_Id;

   ---------------
   -- Read_Data --
   ---------------

   function Read_Data (This : MMA8653_Accelerometer) return All_Axes_Data is
      type Conv (As_Array : Boolean := False) is record
         case As_Array is
            when True  =>
               Arr     : I2C_Data (1 .. 6);
            when False =>
               X, Y, Z : Unsigned_16;
         end case;
      end record with Unchecked_Union;

      Status : I2C_Status;
      Data   : Conv;
      Ret    : All_Axes_Data;
   begin
      This.Port.Mem_Read (Addr          => Device_Address,
                          Mem_Addr      => UInt16 (OUT_X_MSB),
                          Mem_Addr_Size => Memory_Size_8b,
                          Data          => Data.Arr,
                          Status        => Status);

      if Status /= Ok then
         --  No error handling...
         raise Program_Error;
      end if;

      Ret.X := Axis_Data (To_Int_16 (Data.X));
      Ret.Y := Axis_Data (To_Int_16 (Data.Y));
      Ret.Z := Axis_Data (To_Int_16 (Data.Z));
      return Ret;
   end Read_Data;


end MMA8653;
