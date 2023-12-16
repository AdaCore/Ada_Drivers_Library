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

with BME280.Internal;

package body BME280.SPI_Sensors is

   procedure Read
     (Self    : BME280_SPI_Sensor'Class;
      Data    : out HAL.UInt8_Array;
      Success : out Boolean);

   procedure Write
     (Self    : BME280_SPI_Sensor'Class;
      Address : Register_Address;
      Data    : HAL.UInt8;
      Success : out Boolean);

   package Sensor is
     new BME280.Internal (BME280_SPI_Sensor'Class, Read, Write);

   -------------------
   -- Check_Chip_Id --
   -------------------

   overriding function Check_Chip_Id
     (Self   : BME280_SPI_Sensor;
      Expect : HAL.UInt8 := 16#60#) return Boolean is
       (Sensor.Check_Chip_Id (Self, Expect));

   ---------------
   -- Configure --
   ---------------

   overriding procedure Configure
     (Self       : BME280_SPI_Sensor;
      Standby    : Standby_Duration := 0.5;
      Filter     : IRR_Filter_Kind := Off;
      SPI_3_Wire : Boolean := False;
      Success    : out Boolean) is
   begin
      Sensor.Configure (Self, Standby, Filter, SPI_3_Wire, Success);
   end Configure;

   overriding function Measuring (Self : BME280_SPI_Sensor) return Boolean is
     (Sensor.Measuring (Self));

   ----------
   -- Read --
   ----------

   procedure Read
     (Self    : BME280_SPI_Sensor'Class;
      Data    : out HAL.UInt8_Array;
      Success : out Boolean)
   is
      use type HAL.UInt8;
      use all type HAL.SPI.SPI_Status;

      Addr : HAL.UInt8;
      Status : HAL.SPI.SPI_Status;
   begin
      Self.SPI_CS.Clear;

      Addr := HAL.UInt8 (Data'First) or 16#80#;
      Self.SPI_Port.Transmit (HAL.SPI.SPI_Data_8b'(1 => Addr), Status);

      if Status = Ok then
         Self.SPI_Port.Receive (HAL.SPI.SPI_Data_8b (Data), Status);
      end if;

      Self.SPI_CS.Set;

      Success := Status = Ok;
   end Read;

   ----------------------
   -- Read_Measurement --
   ----------------------

   overriding procedure Read_Measurement
     (Self    : BME280_SPI_Sensor;
      Value   : out Measurement;
      Success : out Boolean) is
   begin
      Sensor.Read_Measurement (Self, Value, Success);
   end Read_Measurement;

   ----------------------
   -- Read_Calibration --
   ----------------------

   overriding procedure Read_Calibration
     (Self    : in out BME280_SPI_Sensor;
      Success : out Boolean) is
   begin
      Sensor.Read_Calibration (Self, Self.Calibration, Success);
   end Read_Calibration;

   -----------
   -- Reset --
   -----------

   overriding procedure Reset
     (Self    : BME280_SPI_Sensor;
      Timer   : not null HAL.Time.Any_Delays;
      Success : out Boolean) is
   begin
      Sensor.Reset (Self, Timer, Success);
   end Reset;

   -----------
   -- Start --
   -----------

   overriding procedure Start
     (Self        : BME280_SPI_Sensor;
      Mode        : Sensor_Mode := Normal;
      Humidity    : Oversampling_Kind := X1;
      Pressure    : Oversampling_Kind := X1;
      Temperature : Oversampling_Kind := X1;
      Success     : out Boolean) is
   begin
      Sensor.Start (Self, Mode, Humidity, Pressure, Temperature, Success);
   end Start;

   -----------
   -- Write --
   -----------

   procedure Write
     (Self    : BME280_SPI_Sensor'Class;
      Address : Register_Address;
      Data    : HAL.UInt8;
      Success : out Boolean)
   is
      use type HAL.UInt8;
      use all type HAL.SPI.SPI_Status;

      Prefix : constant HAL.UInt8 := HAL.UInt8 (Address) and 16#7F#;
      Status : HAL.SPI.SPI_Status;
   begin
      Self.SPI_CS.Clear;

      Self.SPI_Port.Transmit (HAL.SPI.SPI_Data_8b'(Prefix, Data), Status);

      Self.SPI_CS.Set;

      Success := Status = Ok;
   end Write;

end BME280.SPI_Sensors;
