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

with Ada.Real_Time;
with Ada.Text_IO;

with Ravenscar_Time;

with STM32.Board;
with STM32.Device;
with STM32.Setup;

with BME280.I2C;

procedure Main is
   use type Ada.Real_Time.Time;

   package BME280_I2C is new BME280.I2C
     (I2C_Port    => STM32.Device.I2C_1'Access,
      I2C_Address => 16#76#);

   Next : Ada.Real_Time.Time := Ada.Real_Time.Clock;

   Ok          : Boolean;
   Calib       : BME280.Calibration_Constants;
   Measurement : BME280.Measurement;
   Temp        : BME280.Deci_Celsius;
   Humi        : BME280.Relative_Humidity;
   Press       : BME280.Pressure_Pa;

begin
   STM32.Board.Initialize_LEDs;
   STM32.Setup.Setup_I2C_Master
     (Port        => STM32.Device.I2C_1,
      SDA         => STM32.Device.PB9,
      SCL         => STM32.Device.PB8,
      SDA_AF      => STM32.Device.GPIO_AF_I2C1_4,
      SCL_AF      => STM32.Device.GPIO_AF_I2C1_4,
      Clock_Speed => 400_000);

   --  Look for BME280 chip
   if not BME280_I2C.Sensor.Check_Chip_Id then
      Ada.Text_IO.Put_Line ("BME280 not found.");
      raise Program_Error;
   end if;

   --  Reset BME280
   BME280_I2C.Sensor.Reset (Ravenscar_Time.Delays, Ok);
   pragma Assert (Ok);

   --  Read calibration data into Clib
   BME280_I2C.Sensor.Read_Calibration (Calib, Ok);

   --  Consigure IRR filter and minimal incativity delay
   BME280_I2C.Sensor.Configure
     (Standby    => 0.5,
      Filter     => BME280.X16,
      SPI_3_Wire => False,
      Success    => Ok);
   pragma Assert (Ok);

   --  Enable cycling of measurements with given oversamplig
   BME280_I2C.Sensor.Start
     (Mode        => BME280.Normal,
      Humidity    => BME280.X1,
      Pressure    => BME280.X16,
      Temperature => BME280.X2,
      Success     => Ok);

   --  Wait for the first measurement
   Ravenscar_Time.Delays.Delay_Milliseconds
     (BME280.Max_Measurement_Time
        (Humidity    => BME280.X1,
         Pressure    => BME280.X16,
         Temperature => BME280.X2) / 1000 + 1);

   loop
      STM32.Board.Toggle (STM32.Board.D1_LED);

      --  Read raw values from the sensor
      BME280_I2C.Sensor.Read_Measurement (Measurement, Ok);

      if Ok then
         --  Decode temperature, humidity and pressure
         Temp := BME280.Temperature (Measurement, Calib);
         Humi := BME280.Humidity (Measurement, Temp, Calib);
         Press := BME280.Pressure (Measurement, Temp, Calib);

         Ada.Text_IO.Put_Line
           ("T=" & Temp'Image &
              " H=" & Humi'Image &
              " P=" & Press'Image);
      end if;

      Next := Next + Ada.Real_Time.Milliseconds (500);
      delay until Next;
   end loop;
end Main;
