------------------------------------------------------------------------------
--                                                                          --
--                 Copyright (C) 2015-2016, AdaCore                         --
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

--  This program demonstrates digital-to-analog conversion (DAC) output using
--  DMA driven by a timer for the DAC input.

--  The user increments the percentage of the DAC output (VRef+) voltage
--  using the blue User button on the STM32F429 Discovery board. The current
--  percentage is displayed on the LCD. The percentage is converted into an
--  absolute value based on the selected resolution. For example, a digital
--  value of 2048 represents 50% in 12-bit resolution. The DMA controller
--  periodically transfers this digital value to the DAC, driven by a timer.
--  The DAC continuously converts this digital value to an analog voltage.

--  Attach a voltmeter to PA4 (or PA5 if channel 2 is used) to see the voltage
--  changes.

with Last_Chance_Handler;      pragma Unreferenced (Last_Chance_Handler);

with HAL;          use HAL;
with STM32.Board;  use STM32.Board;
with STM32.Device; use STM32.Device;
with STM32.GPIO;   use STM32.GPIO;
with STM32.DAC;    use STM32.DAC;
with STM32.DMA;    use STM32.DMA;
with STM32.Timers; use STM32.Timers;
with LCD_Std_Out;  use LCD_Std_Out;

with System;

procedure Demo_DAC_DMA is

   use type Word;

   ---------------------------------  DMA  ------------------------------------

   --  Note that DAC channel 1 uses DMA controller 1, Stream 5, Channel 7

   Controller : DMA_Controller renames DMA_1;

   Stream : constant DMA_Stream_Selector := Stream_5;

   Selected_DMA_Channel : constant DMA_Channel_Selector := Channel_7;

   --------------------------------  Timer  -----------------------------------

   Selected_Timer : Timer renames Timer_6;

   Selected_Trigger : constant External_Event_Trigger_Selection := Timer_6_Output_Trigger;
   --  NB: must match the timer selected above

   ---------------------------------  DAC  ------------------------------------

   Selected_DAC : Digital_To_Analog_Converter renames DAC_1;

   Output_Channel : constant DAC_Channel := Channel_1;
   --  DAC channel 1 uses DMA controller 1, Stream 5, Channel 7

   DAC_Data_Register_Address : constant System.Address :=
      Data_Address (Selected_DAC, Output_Channel, DAC_Resolution_12_Bits, Right_Aligned);
   --  The address of the "data holding register" within the DAC that will
   --  receive the value of Counts via DMA.

   ----------------------------------------------------------------------------

   Requested_Percentage : Word range 0 .. 100;
   --  User input (via the button) representing the currently requested
   --  percentage of the VRef+ DAC output voltage.

   Counts : Word := 0 with Atomic;
   --  The numeric input to the DAC that ultimately controls the voltage
   --  produced (by the DAC) on the GPIO analog output pin. The value is
   --  computed using the Requested_Percentage variable (set by the user)
   --  and Max_Counts, which is either 4095 (12-bit resolution) or 255 (8-bit
   --  resolution). For example, at 12-bit resolution, to get half of the VRef+
   --  output voltage would require Counts to be 2048.

   procedure Print (Value : Word);
   --  Prints the image of the arg at a fixed location

   procedure Await_Button;
   --  Wait for the user to press and then release the blue user button

   procedure Configure_DAC_GPIO (Output_Channel : DAC_Channel);

   procedure Initialize_DMA;

   procedure Initialize_Timer;

   procedure Initialize_DAC;

   -----------
   -- Print --
   -----------

   procedure Print (Value : Word) is
      Value_Image : constant String := Value'Img;
   begin
      Put (170, 52, Value_Image (2 .. Value_Image'Last) & "   ");
   end Print;

   ------------------
   -- Await_Button --
   ------------------

   procedure Await_Button is
   begin
      Await_Pressed : loop
         exit Await_Pressed when Set (User_Button_Point);
      end loop Await_Pressed;

      Await_Released : loop
         exit Await_Released when not Set (User_Button_Point);
      end loop Await_Released;
   end Await_Button;

   ------------------------
   -- Configure_DAC_GPIO --
   ------------------------

   procedure Configure_DAC_GPIO (Output_Channel : DAC_Channel) is
      Config : GPIO_Port_Configuration;

      Output : constant GPIO_Point := (if Output_Channel = Channel_1
                                       then DAC_Channel_1_IO
                                       else DAC_Channel_2_IO);
   begin
      Enable_Clock (Output);
      Config.Mode := Mode_Analog;
      Config.Resistors := Floating;
      Configure_IO (Output, Config);
   end Configure_DAC_GPIO;

   --------------------
   -- Initialize_DMA --
   --------------------

   procedure Initialize_DMA is
      Config : DMA_Stream_Configuration;
   begin
      Enable_Clock (Controller);

      Reset (Controller, Stream);

      Config.Channel                      := Selected_DMA_Channel;
      Config.Direction                    := Memory_To_Peripheral;
      Config.Memory_Data_Format           := HalfWords;
      Config.Peripheral_Data_Format       := HalfWords;
      Config.Increment_Peripheral_Address := False;
      Config.Increment_Memory_Address     := False;
      Config.Operation_Mode               := Circular_Mode;
      Config.Priority                     := Priority_Very_High;
      Config.FIFO_Enabled                 := False;
      Config.FIFO_Threshold               := FIFO_Threshold_Half_Full_Configuration;
      Config.Memory_Burst_Size            := Memory_Burst_Single;
      Config.Peripheral_Burst_Size        := Peripheral_Burst_Single;
      Configure (Controller, Stream, Config);

      Clear_All_Status (Controller, Stream);

      Configure_Data_Flow
        (Controller,
         Stream,
         Source      => Counts'Address,
         Destination => DAC_Data_Register_Address,
         Data_Count  => 1);  -- 1 halfword

      Enable (Controller, Stream);
   end Initialize_DMA;

   ----------------------
   -- Initialize_Timer --
   ----------------------

   procedure Initialize_Timer is
   begin
      Enable_Clock (Selected_Timer);
      Configure (Selected_Timer, Prescaler => 0, Period => 16#FF#);
      Select_Output_Trigger (Selected_Timer, Source => Update);
   end Initialize_Timer;

   --------------------
   -- Initialize_DAC --
   --------------------

   procedure Initialize_DAC is
   begin
      Configure_DAC_GPIO (Output_Channel);
      Enable_Clock (Selected_DAC);
      Reset (Selected_DAC);
      Select_Trigger (Selected_DAC, Output_Channel, Selected_Trigger);
      Enable_Trigger (Selected_DAC, Output_Channel);
      Enable (Selected_DAC, Output_Channel);
      Enable_DMA (Selected_DAC, Output_Channel);
   end Initialize_DAC;

begin  -- main subprogram
   Initialize_LEDs;
   All_LEDs_Off;
   Configure_User_Button_GPIO;
   LCD_Std_Out.Clear_Screen;

   Initialize_Timer;
   Initialize_DMA;
   Initialize_DAC;

   Enable (Selected_Timer);

   Put (0, 0, "VRef+ is 2.95V"); -- Vdd, measured
   Put (0, 25, "Button advances");
   Put (0, 52, "Current %:");
   loop
      for K in Word range 0 .. 10 loop
         Requested_Percentage := K * 10;
         Print (Requested_Percentage);
         Counts := (Requested_Percentage * Max_12bit_Resolution) / 100;
         --  Counts is transferred to the DAC via DMA

         Await_Button;
      end loop;
   end loop;
end Demo_DAC_DMA;
