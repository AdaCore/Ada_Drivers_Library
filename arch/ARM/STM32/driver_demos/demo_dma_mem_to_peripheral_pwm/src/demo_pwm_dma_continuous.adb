------------------------------------------------------------------------------
--                                                                          --
--                 Copyright (C) 2025, AdaCore                              --
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

with STM32.Board;
with STM32.Device;
with STM32.PWM;
with STM32.Timers;
with STM32.DMA;
with HAL;

procedure Demo_PWM_DMA_Continuous is
   type Data is array (0 .. 200) of HAL.UInt32;
   for Data'Component_Size use 32;
   Bytes_To_Transfer : constant := Data'Length;
   Source_Block : Data;

   Controller : STM32.DMA.DMA_Controller renames STM32.Device.DMA_1;
   Configuration : STM32.DMA.DMA_Stream_Configuration;

   Tx_Channel : constant STM32.DMA.DMA_Channel_Selector := STM32.DMA.Channel_6;
   Tx_Stream : constant STM32.DMA.DMA_Stream_Selector := STM32.DMA.Stream_0;

   --  See RM0090, section 10.3.3, for the DMA channel request mapping tables
   --  that say which controllers, and which channels and streams on those
   --  controllers, can connect to which devices. For example, it is channel
   --  six and stream null that connect DMA1 to the timer of TIM5_UP, so
   --  we specify those values above.

   Selected_Timer : STM32.Timers.Timer renames STM32.Device.Timer_5;
   Timer_AF : constant STM32.GPIO_Alternate_Function :=
     STM32.Device.GPIO_AF_TIM5_2;

   Output_Channel : constant STM32.Timers.Timer_Channel :=
     STM32.Timers.Channel_2;

   Requested_Frequency : constant STM32.PWM.Hertz := 100;

   LED_Control : STM32.PWM.PWM_Modulator;

begin

   STM32.PWM.Configure_PWM_Timer (Selected_Timer'Access, Requested_Frequency);

   LED_Control.Attach_PWM_Channel
     (Selected_Timer'Access,
      Output_Channel,
      STM32.Board.Green_LED,
      Timer_AF);
   LED_Control.Enable_Output;

   -- Initialize of values
   for Ind in 0 .. 100 loop
      Source_Block (Ind) := LED_Control.Calculate_Compare_Value (Ind);
      Source_Block (Data'Last - Ind) := Source_Block (Ind);
   end loop;

   STM32.Timers.Set_Output_Preload_Enable
     (Selected_Timer, Output_Channel, True);

   STM32.Timers.Configure_DMA (Selected_Timer,
                               STM32.Timers.DMA_Base_CCR2,
                               STM32.Timers.DMA_Burst_Length_1);

   STM32.Timers.Enable_DMA_Source
     (Selected_Timer, STM32.Timers.Timer_DMA_Update);

   STM32.Device.Enable_Clock (Controller);

   STM32.DMA.Reset (Controller, Tx_Stream);

   Configuration.Channel                      := Tx_Channel;
   Configuration.Direction                    := STM32.DMA.Memory_To_Peripheral;
   Configuration.Increment_Peripheral_Address := False;
   Configuration.Increment_Memory_Address     := True;
   Configuration.Peripheral_Data_Format       := STM32.DMA.Words;
   Configuration.Memory_Data_Format           := STM32.DMA.Words;
   Configuration.Operation_Mode               := STM32.DMA.Circular_Mode;
   Configuration.Priority                     := STM32.DMA.Priority_Very_High;
   Configuration.FIFO_Enabled                 := False;

   STM32.DMA.Configure (Controller, Tx_Stream, Configuration);

   STM32.DMA.Start_Transfer
     (Controller,
      Tx_Stream,
      Source      => Source_Block'Address,
      Destination => STM32.PWM.Data_Register_Address (LED_Control),
      Data_Count  => Bytes_To_Transfer);

   STM32.Timers.Enable_Capture_Compare_DMA (Selected_Timer);

   loop
      null;
   end loop;
end Demo_PWM_DMA_Continuous;



