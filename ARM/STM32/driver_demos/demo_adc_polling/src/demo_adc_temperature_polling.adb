------------------------------------------------------------------------------
--                                                                          --
--                  Copyright (C) 2015-2016, AdaCore                        --
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
--     3. Neither the name of STMicroelectronics nor the names of its       --
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

--  This program demonstrates reading the internal temperature sensor value
--  from an ADC unit, using polling.

--  Note that you will likely need to reset the board manually after loading.

with Last_Chance_Handler;  pragma Unreferenced (Last_Chance_Handler);

with STM32.Board;  use STM32.Board;
with STM32.Device; use STM32.Device;

with HAL;         use HAL;
with STM32.ADC;   use STM32.ADC;
with STM32.GPIO;  use STM32.GPIO;

with LCD_Std_Out;

procedure Demo_ADC_Temperature_Polling is

   All_Regular_Conversions : constant Regular_Channel_Conversions :=
     (1 => (Channel     => Temperature_Sensor.Channel,
            Sample_Time => Sample_144_Cycles));  -- needs 10 micros minimum

   V_Sense : Float;
   --  the "sensed voltage", ie the counts returned by the ADC conversion,
   --  after converting to float

   Temperature : Float;
   --  the computed temperature

   V_Sense_At_25_Degrees : constant := 0.76;
   --  Per the F429 and F405/7 datasheets, at 25 degrees Celcius the sensed
   --  voltage will be approx 0.76V, but this is only estimated by the
   --  manufacturer.
   --  See the F429xx datasheet, section 6.3.22, table 82, pg 159

   V_Ref : constant Float := Float (ADC_Supply_Voltage) / 1000.0;  -- ie 3.3
   --  Reference voltage used by the ADC unit, same as Vdd. See section 5.1.2
   --  of the datasheet for the F40x MCUs, for example.

   Max_Count : constant := 4096.0; -- for 12-bit conversion resolution

   V_At_25_Degrees : constant := (V_Sense_At_25_Degrees / V_Ref) * Max_Count;

   Avg_Slope : constant := 2.5;
   --  Per the F429 and F405/7 datasheets, the average slope is 2.5mV per
   --  degree C.
   --  See the F429xx datasheet, section 6.3.22, pg 159.
   --  For use, see the RM, section 13.10, pg 411.

   Successful : Boolean;
   Timed_Out  : exception;

   procedure Print (X, Y : Natural; Value : Word; Suffix : String := "");

   -----------
   -- Print --
   -----------

   procedure Print (X, Y : Natural; Value : Word; Suffix : String := "") is
      Value_Image : constant String := Value'Img;
   begin
      LCD_Std_Out.Put (X, Y, Value_Image (2 .. Value_Image'Last) & Suffix & "   ");
   end Print;

begin
   Initialize_LEDs;

   Enable_Clock (Temperature_Sensor.ADC.all);

   Reset_All_ADC_Units;

   Configure_Common_Properties
     (Mode           => Independent,
      Prescalar      => PCLK2_Div_2,
      DMA_Mode       => Disabled,
      Sampling_Delay => Sampling_Delay_5_Cycles);

   Configure_Unit
     (Temperature_Sensor.ADC.all,
      Resolution => ADC_Resolution_12_Bits,
      Alignment  => Right_Aligned);

   Configure_Regular_Conversions
     (Temperature_Sensor.ADC.all,
      Trigger     => Software_Triggered,
      Continuous  => False,
      Enable_EOC  => True,
      Conversions => All_Regular_Conversions);

   Enable (ADC_1);

   loop
      Start_Conversion (Temperature_Sensor.ADC.all);

      Poll_For_Status (Temperature_Sensor.ADC.all, Regular_Channel_Conversion_Complete, Successful);
      if not Successful then
         raise Timed_Out;
      end if;

      V_Sense := Float (Conversion_Value (Temperature_Sensor.ADC.all));

      Print (0, 0, Word (V_Sense));

      Temperature := ((V_Sense - V_At_25_Degrees) / Avg_Slope) + 25.0;
      --  see the RM, section 13.10, pg 411

      Print (0, 24, Word (Temperature), " degrees C");

      Toggle (Green);
   end loop;
end Demo_ADC_Temperature_Polling;
