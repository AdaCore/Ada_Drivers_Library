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

with HAL;     use HAL;
with HAL.I2C; use HAL.I2C;

package MMA8653 is

   type MMA8653_Accelerometer (Port : not null Any_I2C_Port) is tagged limited private;


   function Check_Device_Id (This : MMA8653_Accelerometer) return Boolean;
   --  Return False if device ID in incorrect or cannot be read

   type Axis_Data is range -2 ** 9 .. 2 **  10 - 1;

   type All_Axes_Data is record
      X, Y, Z : Axis_Data;
   end record;

   function Read_Data (This : MMA8653_Accelerometer) return All_Axes_Data;

private
   type MMA8653_Accelerometer (Port : not null Any_I2C_Port) is tagged limited
     null record;

   type Register_Addresss is new UInt8;

   Device_Id  : constant := 16#5A#;

   Device_Address : constant I2C_Address := 16#3A# / 2;

   STATUS    : constant Register_Addresss := 16#00#;
   OUT_X_MSB : constant Register_Addresss := 16#01#;
   OUT_X_LSB : constant Register_Addresss := 16#02#;
   OUT_Y_MSB : constant Register_Addresss := 16#03#;
   OUT_Y_LSB : constant Register_Addresss := 16#04#;
   OUT_Z_MSB : constant Register_Addresss := 16#05#;
   OUT_Z_LSB : constant Register_Addresss := 16#06#;
   Who_Am_I : constant Register_Addresss := 16#0D#;

end MMA8653;
