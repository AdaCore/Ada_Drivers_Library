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

with HAL.Time;

package BME280.Sensors is

   type Sensor is limited interface;

   function Check_Chip_Id
     (Self   : Sensor;
      Expect : HAL.UInt8 := 16#60#) return Boolean is abstract;
   --  Read the chip ID and check that it matches

   procedure Reset
     (Self    : Sensor;
      Timer   : not null HAL.Time.Any_Delays;
      Success : out Boolean) is abstract;
   --  Issue a soft reset and wait until the chip is ready.

   procedure Configure
     (Self       : Sensor;
      Standby    : Standby_Duration := 0.5;
      Filter     : IRR_Filter_Kind := Off;
      SPI_3_Wire : Boolean := False;
      Success    : out Boolean) is abstract;
   --  Configure the sensor to use IRR filtering and/or SPI 3-wire mode

   procedure Start
     (Self        : Sensor;
      Mode        : Sensor_Mode := Normal;
      Humidity    : Oversampling_Kind := X1;
      Pressure    : Oversampling_Kind := X1;
      Temperature : Oversampling_Kind := X1;
      Success     : out Boolean) is abstract;
   --  Change sensor mode. Mainly used to start one measurement or enable
   --  perpetual cycling of measurements and inactive periods.

   function Measuring (Self : Sensor) return Boolean is abstract;
   --  Check if a measurement is in progress

   procedure Read_Measurement
     (Self    : Sensor;
      Value   : out Measurement;
      Success : out Boolean) is abstract;
   --  Read the raw measurement values from the sensor

   function Temperature
     (Self  : Sensor;
      Value : Measurement) return Deci_Celsius is abstract;
   --  Get the temperature from raw values in 0.1 Celsius

   function Humidity
     (Self        : Sensor;
      Value       : Measurement;
      Temperature : Deci_Celsius) return Relative_Humidity is abstract;
   --  Get the humidity from raw values

   function Pressure
     (Self        : Sensor;
      Value       : Measurement;
      Temperature : Deci_Celsius) return Pressure_Pa is abstract;
   --  Get the pressure from raw values

   procedure Read_Calibration
     (Self    : in out Sensor;
      Success : out Boolean) is abstract;
   --  Read the calibration constants from the sensor

end BME280.Sensors;
