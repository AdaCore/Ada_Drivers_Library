------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2017, AdaCore                          --
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

--  This program demonstrates reading the VBat (battery voltage) value from
--  an ADC unit, using a timer to drive the ADC conversions and DMA
--  to transfer the raw counts from the ADC unit to the application variable.

with Last_Chance_Handler;  pragma Unreferenced (Last_Chance_Handler);

with STM32.Device;  use STM32.Device;
with STM32.Board;   use STM32.Board;
with HAL;           use HAL;
with STM32.ADC;     use STM32.ADC;
with STM32.DMA;     use STM32.DMA;
with STM32.GPIO;    use STM32.GPIO;
with STM32.Timers;  use STM32.Timers;
with STM32.PWM;     use STM32.PWM;
with Ada.Real_Time; use Ada.Real_Time;
with LCD_Std_Out;

procedure Demo_ADC_Timer_DMA is

   Selected_Timer : Timer renames Timer_1;

   Triggering_Signal : PWM_Modulator;
   --  the PWM modulator used to generate the square wave that triggers the ADC
   --  conversions

   Controller : DMA_Controller renames DMA_2;
   Stream     : constant DMA_Stream_Selector := Stream_0;

   Counts : UInt32 with Volatile;
   --  The raw counts from the ADC sampling the VBat channel. The value is
   --  updated by the DMA controller.

   procedure Configure_ADC;
   --  Set up ADC1 to read the VBat voltage value. Note that only ADC1 can
   --  do this. Conversions are not triggered by software but are instead
   --  triggered by Timer1. Each conversion generates an interrupt signalling
   --  conversion complete.

   procedure Configure_Timer;
   --  Set up Timer1 to generare a square wave of arbitrary frequency, in
   --  order to trigger the ADC conversions. The timer is configured as a PWM
   --  generator since that is the waveform required. The duty cycle determines
   --  the number of rising/falling edges, and it is the edge(s) that triggers
   --  the ADC, so the frequency and duty cycle are significant.

   procedure Configure_DMA;

   function Voltage return UInt32;

   procedure Print (X, Y : Natural; Value : UInt32; Suffix : String := "")
     with Inline;
   --  Print the image of Value at the location indicated, with the suffix
   --  appended

   function Voltage return UInt32 is
      Result : UInt32;
   begin
      Result := ((Counts * VBat_Bridge_Divisor) * ADC_Supply_Voltage) / 16#FFF#;
      --  16#FFF# because we are using 12-bit conversion resolution
      return Result;
   end Voltage;

   -----------
   -- Print --
   -----------

   procedure Print (X, Y : Natural; Value : UInt32; Suffix : String := "") is
      Value_Image : constant String := Value'Img;
   begin
      LCD_Std_Out.Put (X, Y, Value_Image (2 .. Value_Image'Last) & Suffix);
   end Print;

   -------------------
   -- Configure_ADC --
   -------------------

   procedure Configure_ADC is
      All_Regular_Conversions : constant Regular_Channel_Conversions :=
         (1 => (Channel     => VBat.Channel,
                Sample_Time => Sample_3_Cycles));  -- arbitrary
   begin
      Enable_Clock (VBat.ADC.all);

      Configure_Common_Properties
        (Mode           => Independent,
         Prescalar      => PCLK2_Div_2,
         DMA_Mode       => Disabled,
         Sampling_Delay => Sampling_Delay_5_Cycles);  -- arbitrary

      Configure_Unit
        (VBat.ADC.all,
         Resolution => ADC_Resolution_12_Bits,
         Alignment  => Right_Aligned);

      Configure_Regular_Conversions
        (VBat.ADC.all,
         Continuous  => False,  -- False is ESSENTIAL when externally triggered!
         Trigger     => (Trigger_Rising_Edge, Event => Timer1_CC1_Event),
         Enable_EOC  => True,
         Conversions => All_Regular_Conversions);
      --  Either rising or falling edge should work. Note that the Event must
      --  match the timer used!

      Enable_DMA (VBat.ADC.all);

      Enable_DMA_After_Last_Transfer (VBat.ADC.all);
   end Configure_ADC;

   ---------------------
   -- Configure_Timer --
   ---------------------

   procedure Configure_Timer is
      Timer_AF       : constant STM32.GPIO_Alternate_Function := GPIO_AF_TIM1_1;
      Output_Channel : constant Timer_Channel := Channel_1; --  must match ADC trigger
      Frequency      : constant Hertz := 10_000;  -- arbitrary
      Output_Pin     : constant GPIO_Point := PA8;  -- timer 1, channel 1
      --  note that the output pin is not used since we are reading the
      --  internal VBat channel, which is ADC1_IN18
   begin
      Configure_PWM_Timer (Selected_Timer'Access, Frequency);

      Triggering_Signal.Attach_PWM_Channel
        (Selected_Timer'Access,
         Output_Channel,
         Output_Pin,
         Timer_AF);

      Triggering_Signal.Set_Duty_Cycle (Value => 10);
      --  An arbitrary percentage, but determines the number of rising/falling
      --  transitions
   end Configure_Timer;

   -------------------
   -- Configure_DMA --
   -------------------

   procedure Configure_DMA is
      Config : DMA_Stream_Configuration;
   begin
      Enable_Clock (Controller);

      Reset (Controller, Stream);

      Config.Channel                      := Channel_0;
      Config.Direction                    := Peripheral_To_Memory;
      Config.Memory_Data_Format           := HalfWords;
      Config.Peripheral_Data_Format       := HalfWords;
      Config.Increment_Peripheral_Address := False;
      Config.Increment_Memory_Address     := False;
      Config.Operation_Mode               := Circular_Mode;
      Config.Priority                     := Priority_Very_High;
      Config.FIFO_Enabled                 := False;
      Config.Memory_Burst_Size            := Memory_Burst_Single;
      Config.Peripheral_Burst_Size        := Peripheral_Burst_Single;

      Configure (Controller, Stream, Config);

      Clear_All_Status (Controller, Stream);
   end Configure_DMA;

begin
   Initialize_LEDs;

   delay until Clock + Milliseconds (1000);
   --  Give the LCD time to get ready after the unit's elaboration-based
   --  initialization. This is only necessary when the display prints garbage,
   --  as if out of synch somehow. After it starts printing junk the full
   --  second is apparently required. But once it starts working correctly the
   --  delay can be reduced to nothing but don't be foooled by that, it can
   --  come back. TODO: fix the LCD driver...

   LCD_Std_Out.Clear_Screen;

   Configure_ADC;
   Configure_Timer;
   Configure_DMA;

   Start_Transfer
     (Controller,
      Stream,
      Source      => Data_Register_Address (VBat.ADC.all),
      Destination => Counts'Address,
      Data_Count  => 1);  -- ie, 1 halfword

   Enable (VBat.ADC.all);
   Triggering_Signal.Enable_Output;

   loop
      Print (0, 24, Voltage, "mv");
      delay until Clock + Milliseconds (100);  -- arbitrary
   end loop;
end Demo_ADC_Timer_DMA;
