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

pragma Ada_2022;

with Ada.Unchecked_Conversion;

package body BME280 is

   OS_Map : constant array (Oversampling_Kind) of Natural :=
     [Skip => 0, X1 => 1, X2 => 2, X4 => 4, X8 => 8, X16 => 16];
   --  Map Oversamping_Kind to the integer value

   package body Generic_Sensor is

      -------------------
      -- Check_Chip_Id --
      -------------------

      function Check_Chip_Id (Expect : HAL.UInt8 := 16#60#) return Boolean is
         use type HAL.UInt8_Array;

         Ok   : Boolean;
         Data : HAL.UInt8_Array (16#D0# .. 16#D0#);
      begin
         Read (Data, Ok);

         return Ok and Data = [Expect];
      end Check_Chip_Id;

      ---------------
      -- Configure --
      ---------------

      procedure Configure
        (Standby    : Standby_Duration := 1000.0;
         Filter     : IRR_Filter_Kind := Off;
         SPI_3_Wire : Boolean := False;
         Success    : out Boolean)
      is
         use type HAL.UInt8;
         Data : HAL.UInt8;
      begin
         if Standby = 0.5 then
            Data := 0;
         elsif Standby = 20.0 then
            Data := 7;
         elsif Standby = 10.0 then
            Data := 6;
         else
            Data := 5;
            declare
               Value : Standby_Duration := 1000.0;
            begin
               while Value > Standby loop
                  Value := Value / 2;
                  Data := Data - 1;
               end loop;
            end;
         end if;
         Data := Data * 8 + IRR_Filter_Kind'Pos (Filter);
         Data := Data * 4 + Boolean'Pos (SPI_3_Wire);

         Write ([16#F5# => Data], Success);
      end Configure;

      ---------------
      -- Measuring --
      ---------------

      function Measuring return Boolean is
         use type HAL.UInt8;

         Ok   : Boolean;
         Data : HAL.UInt8_Array (16#F3# .. 16#F3#);
      begin
         Read (Data, Ok);

         return Ok and (Data (Data'First) and 8) /= 0;
      end Measuring;

      ----------------------
      -- Read_Calibration --
      ----------------------

      procedure Read_Calibration
        (Value   : out Calibration_Constants;
         Success : out Boolean)
      is
         use Interfaces;

         function Cast is new Ada.Unchecked_Conversion
           (Unsigned_16, Integer_16);

         function To_Unsigned (LSB, MSB : HAL.UInt8) return Unsigned_16 is
           (Unsigned_16 (LSB) + Shift_Left (Unsigned_16 (MSB), 8));

         function To_Integer (LSB, MSB : HAL.UInt8) return Integer_16 is
            (Cast (To_Unsigned (LSB, MSB)));
      begin
         declare
            Data : HAL.UInt8_Array (16#88# .. 16#A1#);
         begin
            Read (Data, Success);

            if not Success then
               return;
            end if;

            Value.T1 := To_Unsigned (Data (16#88#), Data (16#89#));
            Value.T2 := To_Integer (Data (16#8A#), Data (16#8B#));
            Value.T3 := To_Integer (Data (16#8C#), Data (16#8D#));

            Value.P1 := To_Unsigned (Data (16#8E#), Data (16#8F#));
            Value.P2 := To_Integer (Data (16#90#), Data (16#91#));
            Value.P3 := To_Integer (Data (16#92#), Data (16#93#));
            Value.P4 := To_Integer (Data (16#94#), Data (16#95#));
            Value.P5 := To_Integer (Data (16#96#), Data (16#97#));
            Value.P6 := To_Integer (Data (16#98#), Data (16#99#));
            Value.P7 := To_Integer (Data (16#9A#), Data (16#9B#));
            Value.P8 := To_Integer (Data (16#9C#), Data (16#9D#));
            Value.P9 := To_Integer (Data (16#9E#), Data (16#9F#));

            Value.H1 := Unsigned_8 (Data (16#A1#));
         end;

         declare
            use type HAL.UInt8;
            Data : HAL.UInt8_Array (16#E1# .. 16#E7#);
         begin
            Read (Data, Success);

            if not Success then
               return;
            end if;

            Value.H2 := To_Integer (Data (16#E1#), Data (16#E2#));
            Value.H3 := Unsigned_8 (Data (16#E3#));

            Value.H4 := Shift_Left (Unsigned_16 (Data (16#E4#)), 4) +
              Unsigned_16 (Data (16#E5#) and 16#0F#);

            Value.H5 := Shift_Right (Unsigned_16 (Data (16#E5#)), 4) +
              Shift_Left (Unsigned_16 (Data (16#E6#)), 4);

            Value.H6 := Unsigned_8 (Data (16#E7#));
         end;

      end Read_Calibration;

      ----------------------
      -- Read_Measurement --
      ----------------------

      procedure Read_Measurement
        (Value   : out Measurement;
         Success : out Boolean)
      is
         use Interfaces;
         Data : HAL.UInt8_Array (16#F7# .. 16#FE#);
      begin
         Read (Data, Success);

         if Success then
            Value.Raw_Press := HAL.UInt20
              (Shift_Left    (Unsigned_32 (Data (16#F7#)), 12)
               + Shift_Left  (Unsigned_32 (Data (16#F8#)), 4)
               + Shift_Right (Unsigned_32 (Data (16#F9#)), 4));

            Value.Raw_Temp := HAL.UInt20
              (Shift_Left    (Unsigned_32 (Data (16#FA#)), 12)
               + Shift_Left  (Unsigned_32 (Data (16#FB#)), 4)
               + Shift_Right (Unsigned_32 (Data (16#FC#)), 4));

            Value.Raw_Hum := HAL.UInt16
              (Shift_Left (Unsigned_16 (Data (16#FD#)), 8)
               + Unsigned_16 (Data (16#FE#)));
         end if;
      end Read_Measurement;

      -----------
      -- Reset --
      -----------

      procedure Reset
        (Timer   : not null HAL.Time.Any_Delays;
         Success : out Boolean)
      is
         use type HAL.UInt8;

         Data : HAL.UInt8_Array (16#F3# .. 16#F3#);
      begin
         Write ([16#E0# => 16#B6#], Success);

         if not Success then
            return;
         end if;

         for J in 1 .. 3 loop
            Timer.Delay_Milliseconds (2);
            Read (Data, Success);

            if Success and then (Data (Data'First) and 1) = 0 then
               return;
            end if;
         end loop;

         Success := False;
      end Reset;

      -----------
      -- Start --
      -----------

      procedure Start
        (Mode        : Sensor_Mode := Normal;
         Humidity    : Oversampling_Kind := X1;
         Pressure    : Oversampling_Kind := X1;
         Temperature : Oversampling_Kind := X1;
         Success     : out Boolean)
      is
         use type HAL.UInt8;
         Data : HAL.UInt8;
      begin
         Write ([16#F2# => Oversampling_Kind'Pos (Humidity)], Success);

         if Success then
            Data := Oversampling_Kind'Pos (Temperature);
            Data := Data * 8 + Oversampling_Kind'Pos (Pressure);
            Data := Data * 4 + Sensor_Mode'Enum_Rep (Mode);
            Write ([16#F4# => Data], Success);
         end if;
      end Start;

   end Generic_Sensor;

   --------------
   -- Humidity --
   --------------

   function Humidity
     (Value       : Measurement;
      Temperature : Deci_Celsius;
      Calibration : Calibration_Constants) return Relative_Humidity
   is
      Var_1 : constant Integer :=
        Integer (Temperature / Deci_Celsius'Small) - 76800;
      Var_2 : Integer := Integer (Value.Raw_Hum) * 16384;
      Var_3 : Integer := Integer (Calibration.H4) * 1048576;
      Var_4 : Integer := Integer (Calibration.H5) * Var_1;
      Var_5 : Integer := (Var_2 - Var_3 - Var_4 + 16384) / 32768;

   begin
      --  var2 = (var1 * ((int32_t)calib_data->dig_h6)) / 1024;
      Var_2 := Var_1 * Integer (Calibration.H6) / 1024;
      --  var3 = (var1 * ((int32_t)calib_data->dig_h3)) / 2048;
      Var_3 := Var_1 * Integer (Calibration.H3) / 2048;
      --  var4 = ((var2 * (var3 + (int32_t)32768)) / 1024) + (int32_t)2097152;
      Var_4 := Var_2 * (Var_3 + 32768) / 1024 + 2097152;
      --  var2 = ((var4 * ((int32_t)calib_data->dig_h2)) + 8192) / 16384;
      Var_2 := (Var_4 * Integer (Calibration.H2) + 8192) / 16384;
      --  var3 = var5 * var2;
      Var_3 := Var_5 * Var_2;
      --  var4 = ((var3 / 32768) * (var3 / 32768)) / 128;
      Var_4 := (Var_3 / 32768)**2 / 128;
      --  var5 = var3 - ((var4 * ((int32_t)calib_data->dig_h1)) / 16);
      Var_5 := Var_3  - Var_4 * Integer (Calibration.H1) / 16;
      Var_5 := Integer'Max (0, Integer'Min (100 * 2 ** 10, Var_5 / 4096));

      return Relative_Humidity'Small * Var_5;
   end Humidity;

   --------------------------
   -- Max_Measurement_Time --
   --------------------------

   function Max_Measurement_Time
     (Humidity    : Oversampling_Kind := X1;
      Pressure    : Oversampling_Kind := X1;
      Temperature : Oversampling_Kind := X1) return Positive is
        (1_250
         + (if Temperature = Skip then 0 else 2_300 * OS_Map (Temperature))
         + (if Pressure = Skip then 0 else 2_300 * OS_Map (Pressure) + 575)
         + (if Humidity = Skip then 0 else 2_300 * OS_Map (Humidity) + 575));

   --------------
   -- Pressure --
   --------------

   function Pressure
     (Value       : Measurement;
      Temperature : Deci_Celsius;
      Calibration : Calibration_Constants) return Pressure_Pa
   is
      use Interfaces;

      Var_1 : Integer_64 :=
        Integer_64 (Temperature / Deci_Celsius'Small) - 128000;

      Var_2 : Integer_64 := Var_1**2 * Integer_64 (Calibration.P6);
      Var_3 : constant Integer_64 := 140737488355328;
      Var_4 : Integer_64 := 1048576 - Integer_64 (Value.Raw_Press);

      Result : Pressure_Pa'Base;
   begin
      Var_2 := Var_2 + Var_1 * Integer_64 (Calibration.P5) * 131072;
      Var_2 := Var_2 + Integer_64 (Calibration.P4) * 34359738368;

      Var_1 := Var_1 ** 2 * Integer_64 (Calibration.P3) / 256 +
        Var_1 * Integer_64 (Calibration.P2) * 4096;

      Var_1 := (Var_3 + Var_1) * Integer_64 (Calibration.P1) / 8589934592;

      if Var_1 = 0 then
         return Pressure_Pa'First;
      end if;

      Var_4 := (Var_4 * 2147483648 - Var_2) * 3125 / Var_1;
      Var_1 := Integer_64 (Calibration.P9) * (Var_4 / 8192)**2 / 33554432;
      Var_2 := Integer_64 (Calibration.P8) * Var_4 / 524288;

      Var_4 := (Var_4 + Var_1 + Var_2) / 256
        + Integer_64 (Calibration.P7) * 16;

      Result := Pressure_Pa'Small * Integer (Var_4);

      if Result < Pressure_Pa'First then
         return Pressure_Pa'First;
      elsif Result > Pressure_Pa'Last then
         return Pressure_Pa'Last;
      else
         return Result;
      end if;
   end Pressure;

   -----------------
   -- Temperature --
   -----------------

   function Temperature
     (Value       : Measurement;
      Calibration : Calibration_Constants) return Deci_Celsius
   is
      Diff : constant Integer :=
        Integer (Value.Raw_Temp) / 8 - 2 * Integer (Calibration.T1);

      Val_1 : constant Integer :=
        (Diff * Integer (Calibration.T2)) / 2 ** 11;

      Val_2 : constant Integer :=
        (Diff / 2) ** 2 / 2 ** 12 * Integer (Calibration.T3) / 2 ** 14;

   begin
      return Deci_Celsius'Small * (Val_1 + Val_2);
   end Temperature;

   ------------------------------
   -- Typical_Measurement_Time --
   ------------------------------

   function Typical_Measurement_Time
     (Humidity    : Oversampling_Kind := X1;
      Pressure    : Oversampling_Kind := X1;
      Temperature : Oversampling_Kind := X1) return Positive is
        (1_000
         + (if Temperature = Skip then 0 else 2_000 * OS_Map (Temperature))
         + (if Pressure = Skip then 0 else 2_000 * OS_Map (Pressure) + 500)
         + (if Humidity = Skip then 0 else 2_000 * OS_Map (Humidity) + 500));

end BME280;
