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

package body BME280.Internal is

   -------------------
   -- Check_Chip_Id --
   -------------------

   function Check_Chip_Id
     (Device : Device_Context;
      Expect : HAL.UInt8) return Boolean
   is
      use type HAL.UInt8_Array;

      Ok   : Boolean;
      Data : HAL.UInt8_Array (16#D0# .. 16#D0#);
   begin
      Read (Device, Data, Ok);

      return Ok and Data = [Expect];
   end Check_Chip_Id;

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (Device     : Device_Context;
      Standby    : Standby_Duration;
      Filter     : IRR_Filter_Kind;
      SPI_3_Wire : Boolean;
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

      Write (Device, 16#F5#, Data, Success);
   end Configure;

   ---------------
   -- Measuring --
   ---------------

   function Measuring (Device : Device_Context) return Boolean is
      use type HAL.UInt8;

      Ok   : Boolean;
      Data : HAL.UInt8_Array (16#F3# .. 16#F3#);
   begin
      Read (Device, Data, Ok);

      return Ok and (Data (Data'First) and 8) /= 0;
   end Measuring;

   ----------------------
   -- Read_Calibration --
   ----------------------

   procedure Read_Calibration
     (Device  : Device_Context;
      Value   : out Calibration_Constants;
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
         Read (Device, Data, Success);

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
         Read (Device, Data, Success);

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
     (Device  : Device_Context;
      Value   : out Measurement;
      Success : out Boolean)
   is
      use Interfaces;
      Data : HAL.UInt8_Array (16#F7# .. 16#FE#);
   begin
      Read (Device, Data, Success);

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
     (Device  : Device_Context;
      Timer   : not null HAL.Time.Any_Delays;
      Success : out Boolean)
   is
      use type HAL.UInt8;

      Data : HAL.UInt8_Array (16#F3# .. 16#F3#);
   begin
      Write (Device, 16#E0#, 16#B6#, Success);

      if not Success then
         return;
      end if;

      for J in 1 .. 3 loop
         Timer.Delay_Milliseconds (2);
         Read (Device, Data, Success);

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
     (Device      : Device_Context;
      Mode        : Sensor_Mode;
      Humidity    : Oversampling_Kind;
      Pressure    : Oversampling_Kind;
      Temperature : Oversampling_Kind;
      Success     : out Boolean)
   is
      use type HAL.UInt8;
      Data : HAL.UInt8;
   begin
      Write (Device, 16#F2#, Oversampling_Kind'Pos (Humidity), Success);

      if Success then
         Data := Oversampling_Kind'Pos (Temperature);
         Data := Data * 8 + Oversampling_Kind'Pos (Pressure);
         Data := Data * 4 + Sensor_Mode'Enum_Rep (Mode);
         Write (Device, 16#F4#, Data, Success);
      end if;
   end Start;


end BME280.Internal;
