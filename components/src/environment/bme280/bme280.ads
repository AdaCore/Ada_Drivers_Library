------------------------------------------------------------------------------
--                                                                          --
--                       Copyright (C) 2023, AdaCore                        --
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

with Interfaces;
with HAL;
with HAL.Time;

package BME280 is
   pragma Preelaborate;
   pragma Discard_Names;

   type Calibration_Constants is record
      T1 : Interfaces.Unsigned_16;
      T2 : Interfaces.Integer_16;
      T3 : Interfaces.Integer_16;
      P1 : Interfaces.Unsigned_16;
      P2 : Interfaces.Integer_16;
      P3 : Interfaces.Integer_16;
      P4 : Interfaces.Integer_16;
      P5 : Interfaces.Integer_16;
      P6 : Interfaces.Integer_16;
      P7 : Interfaces.Integer_16;
      P8 : Interfaces.Integer_16;
      P9 : Interfaces.Integer_16;
      H1 : Interfaces.Unsigned_8;
      H2 : Interfaces.Integer_16;
      H3 : Interfaces.Unsigned_8;
      H4 : Interfaces.Unsigned_16 range 0 .. 4095;
      H5 : Interfaces.Unsigned_16 range 0 .. 4095;
      H6 : Interfaces.Unsigned_8;
   end record;
   --  Calibration constants per chip. Make visible to allow constant
   --  initialised to a value known in advance.

   type Measurement is private;
   --  Raw values from the sensor

   type Deci_Celsius is delta 1.0 / 2 ** 9 range -99_0.00 .. 99_0.00;
   --  1 degree celsius is 10 Deci_Celsius

   function Temperature
     (Value       : Measurement;
      Calibration : Calibration_Constants) return Deci_Celsius;
   --  Get the temperature from raw values in 0.1 Celsius

   Humidity_Small : constant := 1.0 / 2 ** 10;

   type Relative_Humidity is delta Humidity_Small range 0.0 .. 100.0;
   --  Relative humidity in percent

   function Humidity
     (Value       : Measurement;
      Temperature : Deci_Celsius;
      Calibration : Calibration_Constants) return Relative_Humidity;
   --  Get the humidity from raw values

   Pressure_Small : constant := 1.0 / 2 ** 8;

   type Pressure_Pa is delta Pressure_Small range 30_000.0 .. 110_000.0;
   --  Pressure in Pa

   function Pressure
     (Value       : Measurement;
      Temperature : Deci_Celsius;
      Calibration : Calibration_Constants) return Pressure_Pa;
   --  Get the pressure from raw values

   type Oversampling_Kind is (Skip, X1, X2, X4, X8, X16);
   type IRR_Filter_Kind is (Off, X2, X4, X8, X16);
   type Sensor_Mode is (Sleep, Forced, Normal);
   --  Sensor modes. Sleep - sensor is off, Forced - measure once and go to
   --  sleep, Normal - measure continuously.

   type Standby_Duration is delta 0.5 range 0.5 .. 1000.0
     with Static_Predicate =>
       Standby_Duration in 0.5 | 10.0 | 20.0
         | 62.5 | 125.0 | 250.0 | 500.0 | 1000.0;
   --  Inactivity duration in ms

   subtype Register_Address is Natural range 16#80# .. 16#FF#;
   --  Sensor registers addresses

   function Max_Measurement_Time
     (Humidity    : Oversampling_Kind := X1;
      Pressure    : Oversampling_Kind := X1;
      Temperature : Oversampling_Kind := X1) return Positive;
   --  Maximal measurement time in microseconds

   function Typical_Measurement_Time
     (Humidity    : Oversampling_Kind := X1;
      Pressure    : Oversampling_Kind := X1;
      Temperature : Oversampling_Kind := X1) return Positive;
   --  Typical measurement time in microseconds

   generic
      with procedure Read
        (Data    : out HAL.UInt8_Array;
         Success : out Boolean);
      --  Read the values from the BME280 chip registers into Data.
      --  Each element in the Data corresponds to a specific register address
      --  in the chip, so Data'Range determines the range of registers to read.
      --  The value read from register X will be stored in Data(X), so
      --  Data'Range should be of the Register_Address subtype.

      with procedure Write
        (Data    : HAL.UInt8_Array;
         Success : out Boolean);
      --  Write the values from Data to the BME280 chip registers.
      --  Each element in the Data corresponds to a specific register address
      --  in the chip, so Data'Range determines the range of registers to
      --  write. The value for register X will be read from Data(X), so
      --  Data'Range should be of the Register_Address subtype.

   package Generic_Sensor is

      function Check_Chip_Id (Expect : HAL.UInt8 := 16#60#) return Boolean;
      --  Read the chip ID and check that it matches

      procedure Reset
        (Timer   : not null HAL.Time.Any_Delays;
         Success : out Boolean);
      --  Issue a soft reset and wait until the chip is ready.

      procedure Configure
        (Standby    : Standby_Duration := 1000.0;
         Filter     : IRR_Filter_Kind := Off;
         SPI_3_Wire : Boolean := False;
         Success    : out Boolean);
      --  Configure the sensor to use IRR filtering and/or SPI 3-wire mode

      procedure Start
        (Mode        : Sensor_Mode := Normal;
         Humidity    : Oversampling_Kind := X1;
         Pressure    : Oversampling_Kind := X1;
         Temperature : Oversampling_Kind := X1;
         Success     : out Boolean);
      --  Change sensor mode. Mainly used to start one measurement or enable
      --  perpetual cycling of measurements and inactive periods.

      function Measuring return Boolean;
      --  Check if a measurement is in progress

      procedure Read_Measurement
        (Value   : out Measurement;
         Success : out Boolean);
      --  Read the raw measurement values from the sensor

      procedure Read_Calibration
        (Value   : out Calibration_Constants;
         Success : out Boolean);
      --  Read the calibration constants from the sensor

   end Generic_Sensor;

private

   for Sensor_Mode use (Sleep => 0, Forced => 1, Normal => 3);

   type Measurement is record
      Raw_Press : HAL.UInt20;
      Raw_Temp  : HAL.UInt20;
      Raw_Hum   : HAL.UInt16;
   end record;

end BME280;
