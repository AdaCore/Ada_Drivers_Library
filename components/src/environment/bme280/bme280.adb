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

package body BME280 is

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
      begin
         declare
            Data : HAL.UInt8_Array (16#88# .. 16#A1#);
         begin
            Read (Data, Success);

            if not Success then
               return;
            end if;

            Value.T1 := Unsigned_16 (Data (16#88#)) +
              Shift_Left (Unsigned_16 (Data (16#89#)), 8);

            Value.T2 := Unsigned_16 (Data (16#8A#)) +
              Shift_Left (Unsigned_16 (Data (16#8B#)), 8);

            Value.T3 := Unsigned_16 (Data (16#8C#)) +
              Shift_Left (Unsigned_16 (Data (16#8D#)), 8);
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

      procedure Reset (Success : out Boolean) is
      begin
         Write ([16#E0# => 16#B6#], Success);
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

   function Temperature
     (Value       : Measurement;
      Calibration : Calibration_Constants) return Deci_Celsius
   is
      Diff : constant Integer :=
        Integer (Value.Raw_Temp) / 8 - 2 * Integer (Calibration.T1);

      Val_1 : constant Integer :=
        (Diff * Integer (Calibration.T2)) / 2 ** 11;

      Val_2 : constant Integer :=
        (Diff / 2) ** 2 / 2 ** 11 * Integer (Calibration.T3) / 2 ** 14;

   begin
      return Deci_Celsius'Small * (Val_1 + Val_2);
   end Temperature;

end BME280;
