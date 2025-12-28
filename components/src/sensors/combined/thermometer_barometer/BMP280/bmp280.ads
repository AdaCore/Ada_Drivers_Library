------------------------------------------------------------------------------
--                                                                          --
--                  Copyright (C) 2025, AdaCore                             --
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

--  Driver for Bmp280 sensor

with HAL;        use HAL;
with HAL.GPIO;   use HAL.GPIO;
with HAL.SPI;    use HAL.SPI;
with Interfaces; use Interfaces;

package BMP280 is

   type Chip_ID is new UInt8;

   type Pressure is delta 0.0001 range 300.0 .. 1100.0; -- hPa
   type Temperature is delta 0.01 range -40.0 .. 85.0;  -- °C

   type Status_Type is private;

   type Power_Mode_Type is (Sleep, Forced, Normal) with Size => UInt2'Size;

   type Precision_Type is
     (Skipped,
      Ultra_Low_Power,
      Low_Power,
      Standard_Resolution,
      High_Resolution,
      Ultra_High_Resolution)
     with Size => UInt3'Size;

   type Time_Standby_Type is delta 0.01 range 0.05 .. 4000.00
     with Static_Predicate =>
       Time_Standby_Type in
         0.5 | 62.5 | 125.0 | 250.0 | 500.0
           | 1000.0 | 2000.0 | 4000.0;
   --  Time_Standby in ms

   type SPI_Mode is (SPI4W, SPI3W);

   type Filter_Type is range 0 .. 7;

   -------------------
   -- BMP280_Device --
   -------------------

   type BMP280_Device is limited private;

   procedure Initialize
     (This : in out BMP280_Device;
      CS   : Any_GPIO_Point;
      SPI  : Any_SPI_Port;
     Error : out Boolean);

   function Get_Chip_ID (This : BMP280_Device) return Chip_ID;

   procedure Reset (This : BMP280_Device);

   function Is_Measuring_Set (Status : Status_Type) return Boolean;
   function Is_Im_Update_Set (Status : Status_Type) return Boolean;

   function Get_Status (This : BMP280_Device) return Status_Type;

   procedure Set_Power_Mode
     (This       : BMP280_Device;
      Power_Mode : Power_Mode_Type);

   procedure Set_Temp_Precision
     (This      : BMP280_Device;
      Precision : Precision_Type);

   procedure Set_Press_Precision
     (This      : BMP280_Device;
      Precision : Precision_Type);

   procedure Set_Time_Standby
     (This         : BMP280_Device;
      Time_Standby : Time_Standby_Type);

   procedure Set_Filter
     (This   : BMP280_Device;
      Filter : Filter_Type);

   function Get_Press (This : BMP280_Device) return Pressure;

   function Get_Temp (This : BMP280_Device) return Temperature;

   procedure Set_SPI_Mode (This : BMP280_Device; Mode : SPI_Mode);

private

   type Register_Size is new UInt8;

   type Status_Type is record
      Im_Update : Bit;
      Reserved1 : UInt2;
      Measuring : Bit;
      Reserved2 : UInt4;
   end record with Size => Register_Size'Size;

   for Status_Type use record
      Im_Update at 0 range 0 .. 0;
      Reserved1 at 0 range 1 .. 2;
      Measuring at 0 range 3 .. 3;
      Reserved2 at 0 range 4 .. 7;
   end record;

   for Power_Mode_Type use
     (Sleep  => 2#00#,
      Forced => 2#01#,
      Normal => 2#11#);

   for Precision_Type use
     (
      Skipped               => 2#000#,
      Ultra_Low_Power       => 2#001#,
      Low_Power             => 2#010#,
      Standard_Resolution   => 2#011#,
      High_Resolution       => 2#100#,
      Ultra_High_Resolution => 2#101#);

   type Ctrl_Meas_Register is record
      mode   : Power_Mode_Type;
      osrs_p : Precision_Type;
      osrs_t : Precision_Type;
   end record with Size => Register_Size'Size;

   for Ctrl_Meas_Register use record
      mode   at 0 range 0 .. 1;
      osrs_p at 0 range 2 .. 4;
      osrs_t at 0 range 5 .. 7;
   end record;

   type Config_Register is record
      spi3w_en : Bit;
      Reserved : Bit;
      filter   : UInt3;
      t_sb     : UInt3;
   end record with Size => Register_Size'Size;

   for Config_Register use record
      spi3w_en at 0 range 0 .. 0;
      Reserved at 0 range 1 .. 1;
      filter   at 0 range 2 .. 4;
      t_sb     at 0 range 5 .. 7;
   end record;

   type Register_Name is
     (id,
      reset,
      status,
      ctrl_meas,
      config,
      press_msb,
      press_lsb,
      press_xlsb,
      temp_msb,
      temp_lsb,
      temp_xlsb,

      dig_T1_lsb,
      dig_T1_msb,
      dig_T2_lsb,
      dig_T2_msb,
      dig_T3_lsb,
      dig_T3_msb,
      dig_P1_lsb,
      dig_P1_msb,
      dig_P2_lsb,
      dig_P2_msb,
      dig_P3_lsb,
      dig_P3_msb,
      dig_P4_lsb,
      dig_P4_msb,
      dig_P5_lsb,
      dig_P5_msb,
      dig_P6_lsb,
      dig_P6_msb,
      dig_P7_lsb,
      dig_P7_msb,
      dig_P8_lsb,
      dig_P8_msb,
      dig_P9_lsb,
      dig_P9_msb);

   type Register_Adresses is range 16#08# .. 16#7C# with Size => UInt7'Size;

   Adresses : constant array (Register_Name) of Register_Adresses :=
     (id         => 16#50#,
      reset      => 16#60#,
      status     => 16#73#,
      ctrl_meas  => 16#74#,
      config     => 16#75#,
      press_msb  => 16#77#,
      press_lsb  => 16#78#,
      press_xlsb => 16#79#,
      temp_msb   => 16#7A#,
      temp_lsb   => 16#7B#,
      temp_xlsb  => 16#7C#,

      dig_T1_lsb => 16#8#,
      dig_T1_msb => 16#9#,
      dig_T2_lsb => 16#A#,
      dig_T2_msb => 16#B#,
      dig_T3_lsb => 16#C#,
      dig_T3_msb => 16#D#,
      dig_P1_lsb => 16#E#,
      dig_P1_msb => 16#F#,
      dig_P2_lsb => 16#10#,
      dig_P2_msb => 16#11#,
      dig_P3_lsb => 16#12#,
      dig_P3_msb => 16#13#,
      dig_P4_lsb => 16#14#,
      dig_P4_msb => 16#15#,
      dig_P5_lsb => 16#16#,
      dig_P5_msb => 16#17#,
      dig_P6_lsb => 16#18#,
      dig_P6_msb => 16#19#,
      dig_P7_lsb => 16#1A#,
      dig_P7_msb => 16#1B#,
      dig_P8_lsb => 16#1C#,
      dig_P8_msb => 16#1D#,
      dig_P9_lsb => 16#1E#,
      dig_P9_msb => 16#1F#);

   type Command_Type is (Write, Read) with Size => Bit'Size;
   for Command_Type use (Write => 0, Read => 1);

   type Command is record
      Cmd  : Command_Type;
      Addr : Register_Adresses;
   end record with Size => UInt8'Size;

   for Command use record
      Addr at 0 range 0 .. 6;
      Cmd  at 0 range 7 .. 7;
   end record;

   procedure CS_Low (This : BMP280_Device);

   procedure CS_High (This : BMP280_Device);

   function Read_Register
     (This     : BMP280_Device;
      Register : Register_Name)
      return UInt8;

   function Read_Register
     (This : BMP280_Device;
      Msb  : Register_Name;
      Lsb  : Register_Name)
      return UInt16;

   function Read_Register
     (This : BMP280_Device;
      Msb  : Register_Name;
      Lsb  : Register_Name)
      return Interfaces.Integer_16;

   procedure Write_Register
     (This     : BMP280_Device;
      Register : Register_Name;
      Data     : UInt8);

   procedure Read_Trim_Registers (This : in out BMP280_Device);

   function Get_T_Fine
     (This  : BMP280_Device;
      Adc_T : Interfaces.Integer_32)
      return Interfaces.Integer_32;

   function Read_Register
     (This      : BMP280_Device;
      Precision : Precision_Type;
      Msb       : Register_Name;
      Lsb       : Register_Name;
      Xlsb      : Register_Name)
      return Interfaces.Integer_32 with Pre => (Precision /= Skipped);

   type Holder_Type is record
      CS      : Any_GPIO_Point;
      SPI     : Any_SPI_Port;

      Chip_ID : Integer_8;
      Dig_T1  : Integer_32;
      Dig_T2  : Integer_32;
      Dig_T3  : Integer_32;
      dig_P1  : Integer_32;
      dig_P2  : Integer_32;
      dig_P3  : Integer_32;
      dig_P4  : Integer_32;
      dig_P5  : Integer_32;
      dig_P6  : Integer_32;
      dig_P7  : Integer_32;
      dig_P8  : Integer_32;
      dig_P9  : Integer_32;
   end record;

   type BMP280_Device is limited record
      Holder : Holder_Type;
   end record;

end BMP280;
