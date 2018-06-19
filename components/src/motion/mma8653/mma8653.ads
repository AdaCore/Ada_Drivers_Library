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

with HAL;        use HAL;
with HAL.I2C;    use HAL.I2C;

private with Ada.Unchecked_Conversion;

package MMA8653 is

   type MMA8653_Accelerometer (Port : not null Any_I2C_Port)
   is tagged limited private;

   type Oversampling_Mode is (Normal, Low_Noise_Low_Power,
                              High_Resolution, Low_Power);

   type Dynamic_Range is (Two_G, Four_G, Heigh_G);

   procedure Configure (This                : in out MMA8653_Accelerometer;
                        Dyna_Range          : Dynamic_Range;
                        Sleep_Oversampling  : Oversampling_Mode;
                        Active_Oversampling : Oversampling_Mode);

   function Check_Device_Id (This : MMA8653_Accelerometer) return Boolean;
   --  Return False if device ID in incorrect or cannot be read

   type Axis_Data is range -2 ** 9 .. 2 ** 9 - 1
     with Size => 10;

   type All_Axes_Data is record
      X, Y, Z : Axis_Data;
   end record;

   function Read_Data (This : MMA8653_Accelerometer) return All_Axes_Data;

private
   type MMA8653_Accelerometer (Port : not null Any_I2C_Port) is tagged limited
     null record;

   for Oversampling_Mode use (Normal              => 0,
                              Low_Noise_Low_Power => 1,
                              High_Resolution     => 2,
                              Low_Power           => 3);

   for Dynamic_Range use (Two_G   => 0,
                          Four_G  => 1,
                          Heigh_G => 2);

   type Register_Addresss is new UInt8;

   Device_Id  : constant := 16#5A#;

   Device_Address : constant I2C_Address := 16#3A#;

   DATA_STATUS : constant Register_Addresss := 16#00#;
   OUT_X_MSB   : constant Register_Addresss := 16#01#;
   OUT_X_LSB   : constant Register_Addresss := 16#02#;
   OUT_Y_MSB   : constant Register_Addresss := 16#03#;
   OUT_Y_LSB   : constant Register_Addresss := 16#04#;
   OUT_Z_MSB   : constant Register_Addresss := 16#05#;
   OUT_Z_LSB   : constant Register_Addresss := 16#06#;
   Who_Am_I    : constant Register_Addresss := 16#0D#;

   XYZ_DATA_CFG : constant Register_Addresss := 16#0E#;

   CTRL_REG1 : constant Register_Addresss := 16#2A#;
   CTRL_REG2 : constant Register_Addresss := 16#2B#;
   CTRL_REG3 : constant Register_Addresss := 16#2C#;
   CTRL_REG4 : constant Register_Addresss := 16#2D#;
   CTRL_REG5 : constant Register_Addresss := 16#2E#;

   type CTRL_REG1_Register is record
      Active    : Boolean := False;
      F_Read    : Boolean := False;
      Reserved  : Bit     := 0;
      Data_Rate : UInt3   := 0;
      ASLP_Rate : UInt2   := 0;
   end record;

   for CTRL_REG1_Register use record
      Active    at 0 range 0 .. 0;
      F_Read    at 0 range 1 .. 1;
      Reserved  at 0 range 2 .. 2;
      Data_Rate at 0 range 3 .. 5;
      ASLP_Rate at 0 range 6 .. 7;
   end record;

   function To_UInt8 is new Ada.Unchecked_Conversion
     (CTRL_REG1_Register, UInt8);
   function To_Reg is new Ada.Unchecked_Conversion
     (UInt8, CTRL_REG1_Register);

   type CTRL_REG2_Register is record
      MODS      : UInt2   := 0;
      SPLE      : Boolean := False;
      SMODS     : UInt2   := 0;
      Reserved  : Bit     := 0;
      Reset     : Boolean := False;
      Self_Test : Boolean := False;
   end record;

   for CTRL_REG2_Register use record
      MODS      at 0 range 0 .. 1;
      SPLE      at 0 range 2 .. 2;
      SMODS     at 0 range 3 .. 4;
      Reserved  at 0 range 5 .. 5;
      Reset     at 0 range 6 .. 6;
      Self_Test at 0 range 7 .. 7;
   end record;

   function To_UInt8 is new Ada.Unchecked_Conversion
     (CTRL_REG2_Register, UInt8);
   function To_Reg is new Ada.Unchecked_Conversion
     (UInt8, CTRL_REG2_Register);

end MMA8653;
