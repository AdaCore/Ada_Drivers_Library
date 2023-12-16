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

--  This package provides a type representing the BME280 connected via the I2C
--  interface.

with HAL.I2C;
with HAL.Time;

with BME280.Sensors;

package BME280.I2C_Sensors is

   Default_Address : constant HAL.UInt7 := 16#76#;
   --  The typical BME280 7-bit I2C address

   type BME280_I2C_Sensor
     (I2C_Port    : not null HAL.I2C.Any_I2C_Port;
      I2C_Address : HAL.UInt7) is limited new BME280.Sensors.Sensor with
   record
      Calibration : Calibration_Constants;
   end record;

   overriding function Check_Chip_Id
     (Self   : BME280_I2C_Sensor;
      Expect : HAL.UInt8 := 16#60#) return Boolean;
   --  Read the chip ID and check that it matches

   overriding procedure Reset
     (Self    : BME280_I2C_Sensor;
      Timer   : not null HAL.Time.Any_Delays;
      Success : out Boolean);
   --  Issue a soft reset and wait until the chip is ready.

   overriding procedure Configure
     (Self       : BME280_I2C_Sensor;
      Standby    : Standby_Duration := 0.5;
      Filter     : IRR_Filter_Kind := Off;
      SPI_3_Wire : Boolean := False;
      Success    : out Boolean);
   --  Configure the sensor to use IRR filtering and/or SPI 3-wire mode

   overriding procedure Start
     (Self        : BME280_I2C_Sensor;
      Mode        : Sensor_Mode := Normal;
      Humidity    : Oversampling_Kind := X1;
      Pressure    : Oversampling_Kind := X1;
      Temperature : Oversampling_Kind := X1;
      Success     : out Boolean);
   --  Change sensor mode. Mainly used to start one measurement or enable
   --  perpetual cycling of measurements and inactive periods.

   overriding function Measuring (Self : BME280_I2C_Sensor) return Boolean;
   --  Check if a measurement is in progress

   overriding procedure Read_Measurement
     (Self    : BME280_I2C_Sensor;
      Value   : out Measurement;
      Success : out Boolean);
   --  Read the raw measurement values from the sensor

   overriding function Temperature
     (Self  : BME280_I2C_Sensor;
      Value : Measurement) return Deci_Celsius is
        (Temperature (Value, Self.Calibration));
   --  Get the temperature from raw values in 0.1 Celsius

   overriding function Humidity
     (Self        : BME280_I2C_Sensor;
      Value       : Measurement;
      Temperature : Deci_Celsius) return Relative_Humidity is
        (Humidity (Value, Temperature, Self.Calibration));
   --  Get the humidity from raw values

   overriding function Pressure
     (Self        : BME280_I2C_Sensor;
      Value       : Measurement;
      Temperature : Deci_Celsius) return Pressure_Pa is
        (Pressure (Value, Temperature, Self.Calibration));
   --  Get the pressure from raw values

   overriding procedure Read_Calibration
     (Self    : in out BME280_I2C_Sensor;
      Success : out Boolean);
   --  Read the calibration constants from the sensor

end BME280.I2C_Sensors;
