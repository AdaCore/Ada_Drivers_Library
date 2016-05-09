------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2015, AdaCore                           --
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

--  This program demonstrates using one timer to generate several periodic
--  interrupts, each with a different period.  Each interrupt is tied to a
--  separate LED such that the LED blinks at the corresponding rate.

--  This file declares the main procedure for the demonstration.

--  We use the STM F4 Discovery board for the sake of the four LEDs but another board could be
--  used, with different LED configurations. Using a board with fewer LEDs is
--  possible but less interesting. In that case, change the number of channels
--  driven (and interrupts generated) to match the number of LEDs available.

with Last_Chance_Handler;      pragma Unreferenced (Last_Chance_Handler);

with STM32.Board;              use STM32.Board;
with STM32.Device;             use STM32.Device;
with STM32.Timers;             use STM32.Timers;
with STM32F4_Timer_Interrupts; use STM32F4_Timer_Interrupts;
with HAL;                      use HAL;

procedure Demo is
begin
   Initialize_LEDs;

   --  set up the timer base

   Enable_Clock (Timer_3);

   Reset (Timer_3);

   Configure
     (Timer_3,
      Prescaler     => Prescaler,
      Period        => Word (Short'Last),  -- all the way up
      Clock_Divisor => Div1,
      Counter_Mode  => Up);

   Configure_Prescaler
     (Timer_3,
      Prescaler   => Prescaler,
      Reload_Mode => Immediate);

   --  configure the channel outputs

   for Next_Channel in Timer_Channel loop
      Configure_Channel_Output
        (Timer_3,
         Channel  => Next_Channel,
         Mode     => Timing,
         State    => Enable,
         Pulse    => Channel_Periods (Next_Channel),
         Polarity => High);

      Set_Output_Preload_Enable (Timer_3, Next_Channel, False);
   end loop;

   --  enable the timer's four channel interrupts and go

   Enable_Interrupt
     (Timer_3,
      Timer_CC1_Interrupt & Timer_CC2_Interrupt &
      Timer_CC3_Interrupt & Timer_CC4_Interrupt);

   Enable (Timer_3);

   loop
      null;
   end loop;
end Demo;
