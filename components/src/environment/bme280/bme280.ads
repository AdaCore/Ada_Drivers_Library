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

package BME280 is
   pragma Pure;
   pragma Discard_Names;

   type Calibration_Constants is record
      T1 : Interfaces.Unsigned_16;
      T2 : Interfaces.Unsigned_16;
      T3 : Interfaces.Unsigned_16;
   end record;

   type Measurement is private;

   type Deci_Celsius is delta 1.0 / 2 ** 9 range -99_0.00 .. 99_0.00;
   --  1 degree celsius is 10 Deci_Celsius

   function Temperature
     (Value       : Measurement;
      Calibration : Calibration_Constants) return Deci_Celsius;

   type Oversampling_Kind is (Skip, X1, X2, X4, X8, X16);
   type IRR_Filter_Kind is (Off, X1, X2, X4, X8, X16);
   type Sensor_Mode is (Sleep, Forced, Normal);

   type Standby_Duration is delta 0.5 range 0.5 .. 1000.0
     with Static_Predicate =>
       Standby_Duration in 0.5 | 10.0 | 20.0
         | 62.5 | 125.0 | 250.0 | 500.0 | 1000.0;
   --  Inactive duration in ms

   subtype Register_Address is Natural range 16#80# .. 16#FF#;

   generic
      with procedure Read
        (Data    : out HAL.UInt8_Array;
         Success : out Boolean);
      --  Data'Range should be Register_Address

      with procedure Write
        (Data    : HAL.UInt8_Array;
         Success : out Boolean);
      --  Data'Range should be Register_Address

   package Generic_Sensor is
      function Check_Chip_Id (Expect : HAL.UInt8 := 16#60#) return Boolean;

      procedure Reset (Success : out Boolean);

      procedure Configure
        (Standby    : Standby_Duration := 1000.0;
         Filter     : IRR_Filter_Kind := Off;
         SPI_3_Wire : Boolean := False;
         Success    : out Boolean);

      procedure Start
        (Mode        : Sensor_Mode := Normal;
         Humidity    : Oversampling_Kind := X1;
         Pressure    : Oversampling_Kind := X1;
         Temperature : Oversampling_Kind := X1;
         Success     : out Boolean);

      function Measuring return Boolean;

      procedure Read_Measurement
        (Value   : out Measurement;
         Success : out Boolean);

      procedure Read_Calibration
        (Value   : out Calibration_Constants;
         Success : out Boolean);

   end Generic_Sensor;

private

   for Sensor_Mode use (Sleep => 0, Forced => 1, Normal => 3);

   type Measurement is record
      Raw_Press : HAL.Uint20;
      Raw_Temp  : HAL.Uint20;
      Raw_Hum   : HAL.Uint16;
   end record;

end BME280;
