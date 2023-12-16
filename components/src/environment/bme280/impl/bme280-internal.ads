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

generic
   type Device_Context (<>) is limited private;

   with procedure Read
     (Device  : Device_Context;
      Data    : out HAL.UInt8_Array;
      Success : out Boolean);
   --  Read the values from the BME280 chip registers into Data.
   --  Each element in the Data corresponds to a specific register address
   --  in the chip, so Data'Range determines the range of registers to read.
   --  The value read from register X will be stored in Data(X), so
   --  Data'Range should be of the Register_Address subtype.

   with procedure Write
     (Device  : Device_Context;
      Address : Register_Address;
      Data    : HAL.UInt8;
      Success : out Boolean);
   --  Write the value to the BME280 chip register with given Address.

package BME280.Internal is

   function Check_Chip_Id
     (Device : Device_Context;
      Expect : HAL.UInt8) return Boolean;
   --  Read the chip ID and check that it matches

   procedure Reset
     (Device  : Device_Context;
      Timer   : not null HAL.Time.Any_Delays;
      Success : out Boolean);
   --  Issue a soft reset and wait until the chip is ready.

   procedure Configure
     (Device     : Device_Context;
      Standby    : Standby_Duration;
      Filter     : IRR_Filter_Kind;
      SPI_3_Wire : Boolean;
      Success    : out Boolean);
   --  Configure the sensor to use IRR filtering and/or SPI 3-wire mode

   procedure Start
     (Device      : Device_Context;
      Mode        : Sensor_Mode;
      Humidity    : Oversampling_Kind;
      Pressure    : Oversampling_Kind;
      Temperature : Oversampling_Kind;
      Success     : out Boolean);
   --  Change sensor mode. Mainly used to start one measurement or enable
   --  perpetual cycling of measurements and inactive periods.

   function Measuring (Device  : Device_Context) return Boolean;
   --  Check if a measurement is in progress

   procedure Read_Measurement
     (Device  : Device_Context;
      Value   : out Measurement;
      Success : out Boolean);
   --  Read the raw measurement values from the sensor

   procedure Read_Calibration
     (Device  : Device_Context;
      Value   : out Calibration_Constants;
      Success : out Boolean);
   --  Read the calibration constants from the sensor

end BME280.Internal;
