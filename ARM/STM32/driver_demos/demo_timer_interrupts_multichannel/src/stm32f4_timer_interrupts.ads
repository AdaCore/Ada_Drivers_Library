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

--  This demo uses four channels of a single timer to make four LEDs blink
--  at four different rates, without requiring periodic tasks to drive them.
--  Because it uses four LEDs it runs on the STM32F4_Discovery board. Using
--  a board with fewer LEDs is possible but less interesting. In that case,
--  change the number of channels driven (and interrupts generated) to match
--  the number of LEDs available.

--  The file declares the interrupt handler and the timer values for the
--  demonstration.

with Ada.Interrupts.Names; use Ada.Interrupts.Names;
with Interfaces;           use Interfaces;

with STM32;                use STM32;
with STM32.Device;         use STM32.Device;
with STM32.Timers;         use STM32.Timers;
with HAL;                  use HAL;

package STM32F4_Timer_Interrupts is

   --  These values are set so that we get the four LEDs blinking at the
   --  desired rates indicated below. The specific LEDs (colors) associated
   --  with the rates are arbitrary (i.e., different LEDs could be associated
   --  with the channels). By controlling the rates at which the channels
   --  generate the interrupts we control the rates at which the LEDs blink.

   SystemCoreClock : constant Word := System_Clock_Frequencies.SYSCLK;

   Prescaler : constant Short := Short (((SystemCoreClock / 2) / 6000) - 1);

   Channel_1_Period : constant := 6000 - 1;                          -- 1 sec
   Channel_2_Period : constant := ((Channel_1_Period + 1) / 2) - 1;  -- 1/2 sec
   Channel_3_Period : constant := ((Channel_2_Period + 1) / 2) - 1;  -- 1/4 sec
   Channel_4_Period : constant := ((Channel_3_Period + 1) / 2) - 1;  -- 1/8 sec

   --  A convenience array for the sake of using a loop to configure the timer
   --  channels
   Channel_Periods  : constant array (Timer_Channel) of Word :=
     (Channel_1_Period,
      Channel_2_Period,
      Channel_3_Period,
      Channel_4_Period);

   --  The interrupt handling PO. There is no client API so there is nothing
   --  declared in the visible part. This declaration could reasonably be
   --  moved to the package body, but we leave it here to emphasize the
   --  design approach using interrupts.
   protected Handler is
      pragma Interrupt_Priority;
   private

      --  Whenever the timer interrupt occurs, the handler determines which
      --  channel has caused the interrupt and toggles the corresponding LED.
      --  It then increments the channel's compare value so that the timer
      --  will generate another interrupt on that channel after the required
      --  interval specific to that channel.
      procedure IRQ_Handler;
      pragma Attach_Handler (IRQ_Handler, TIM3_Interrupt);

   end Handler;

end STM32F4_Timer_Interrupts;
