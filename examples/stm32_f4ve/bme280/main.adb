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

with HAL.Bitmap;

with STM32.Board;
with Display_ILI9341;

with BME280.I2C;
with STM32.Device;
with STM32.Setup;
with BMP_Fonts;
with Bitmapped_Drawing;
procedure Main is
   use type Ada.Real_Time.Time;

   procedure Put_Line (Text : String) is
   begin
      STM32.Board.TFT_Bitmap.Set_Source (HAL.Bitmap.Black);
      STM32.Board.TFT_Bitmap.Fill_Rect (Area => ((5, 5),100, 10));

      Bitmapped_Drawing.Draw_String
        (STM32.Board.TFT_Bitmap,
         (5, 5),
         Text,
         BMP_Fonts.Font8x8,
         HAL.Bitmap.Green,
         HAL.Bitmap.Black);
   end Put_Line;

   package BME280_I2C is new BME280.I2C
     (I2C_Port    => STM32.Device.I2C_1'Access,
      I2C_Address => 16#76#);

   Next : Ada.Real_Time.Time := Ada.Real_Time.Clock;

   --  Bitmap : Display_ILI9341.Bitmap_Buffer renames STM32.Board.TFT_Bitmap;

   Ok : Boolean;
   Calib       : BME280.Calibration_Constants;
   Measurement : BME280.Measurement;
   Temp        : BME280.Deci_Celsius;
   Humi        : BME280.Relative_Humidity;

begin
   STM32.Board.Initialize_LEDs;
   STM32.Board.Display.Initialize;
   STM32.Setup.Setup_I2C_Master
     (Port        => STM32.Device.I2C_1,
      SDA         => STM32.Device.PB9,
      SCL         => STM32.Device.PB8,
      SDA_AF      => STM32.Device.GPIO_AF_I2C1_4,
      SCL_AF      => STM32.Device.GPIO_AF_I2C1_4,
      Clock_Speed => 400_000);

   BME280_I2C.Sensor.Read_Calibration (Calib, Ok);
   BME280_I2C.Sensor.Start (Success => Ok);

   loop
      STM32.Board.Toggle (STM32.Board.D1_LED);
      BME280_I2C.Sensor.Read_Measurement (Measurement, Ok);

      if Ok then
         Temp := BME280.Temperature (Measurement, Calib);
         Humi := BME280.Humidity (Measurement, Temp, Calib);
         Put_Line (Temp'Image & Humi'Image);
      end if;

      Next := Next + Ada.Real_Time.Milliseconds (500);
      delay until Next;
   end loop;
end Main;
