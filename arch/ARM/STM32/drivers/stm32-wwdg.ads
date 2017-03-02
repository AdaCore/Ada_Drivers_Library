------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2017, AdaCore                           --
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

--  This package provides an interface to the on-board "window watchdog"
--  provided by the STM32F4 family.

--  Important: see Figure 215 in RM0090, the STM32 reference manual,
--  illustrating the watchdog behavior and significant events.
--  Note that time is increasing to the right in the figure.
--  www.st.com/resource/en/reference_manual/DM00031020.pdf

package STM32.WWDG is  --  the Window Watchdog
   pragma Elaborate_Body;

   procedure Enable_Watchdog_Clock;

   procedure Reset_Watchdog;
   --  Resets the peripheral using the RCC

   type Prescalers is
     (Divider_1,
      Divider_2,
      Divider_4,
      Divider_8)
     with Size => 2;
   --  These prescalers are clock frequency dividers, with numeric divisor
   --  values corresponding to the digits in the names. The dividers are used
   --  to determine the watchdog counter clock frequency driving the countdown
   --  timer.
   --
   --  The frequency is computed as follows:
   --     PCLK1 / 4096 / divider-value
   --  For example, assuming PCLK1 of 45MHz and Divider_8, we'd get:
   --     45_000_000 / 4096 / 8 => 1373 Hz
   --
   --  The frequency can be used to calculate the time span values for the
   --  refresh window, for example. See the demo program for doing that.

   procedure Set_Watchdog_Prescaler (Value : Prescalers);
   --  Set the divider used to derive the watchdog counter clock frequency
   --  driving the countdown timer.
   --
   --  Note that the watchdog counter only counts down to 16#40# (64) before
   --  triggering the reset, and the upper limit for the counter value is 127,
   --  so there is a relatively limited number of counts available for the
   --  refresh window to be open. Therefore, the frequency at which the
   --  watchdog clock drives the counter is a significant factor in the
   --  total possible time span for the refresh window.

   subtype Downcounter is UInt7 range 16#40# .. UInt7'Last;
   --  16#40# is the last value of the countdown counter prior to a reset. When
   --  the count goes to 16#3F# the high-oder bit being cleared triggers the
   --  reset (if the watchdog is activated). Thus we never want to specify a
   --  counter value less than 16#40#.

   procedure Set_Watchdog_Window (Window_Start_Count : Downcounter);
   --  Set the value of the countdown counter for when the refresh window is
   --  to open. In figure 215, this is the counter value at the start of the
   --  "refresh allowed" interval. It is an arbitrary, user-defined value.
   --
   --  The last counter value prior to reset is always 64 (16#40#). At 16#3F#
   --  the high-order counter bit (T6 in the figure) clears, causing the
   --  watchdog to trigger the system reset (when activated).
   --
   --  To prevent a reset, the watchdog counter must be refreshed by the
   --  application but only when the counter is lower than Window_Start_Count
   --  and greater than 16#3F#.

   procedure Activate_Watchdog (New_Count : Downcounter);
   --  Set the watchdog counter to begin counting down from New_Count and
   --  activate the watchdog. In the figure, this is the counter value at
   --  the start of the "refresh not allowed" interval. It is an arbitrary,
   --  user-defined value.
   --
   --  Once activated there is no way to deactivate the watchdog other
   --  than a system reset.
   --
   --  Note that, as a "window" watchdog, the period in which the counter can
   --  be refreshed does not open immediately upon activating the watchdog.

   procedure Refresh_Watchdog_Counter (New_Count : Downcounter);
   --  Set the counter to begin counting down from New_Count. Used to refresh
   --  the watchdog counter in order to prevent the watchdog timeout. In the
   --  figure, this is the counter value at the start of the "refresh not
   --  allowed" interval. It is an arbitrary, user-defined value, but is likely
   --  the same value passed to Activate_Watchdog. The difference between this
   --  routine and the activating routine is soley the activation.

   procedure Enable_Early_Wakeup_Interrupt;
   --  When enabled, an interrupt occurs whenever the counter reaches the value
   --  16#40#, ie one counter decrement period before the counter causes a
   --  system reset. Once enabled, this interrupt can only be disabled by
   --  hardware after a system reset.

   function Early_Wakeup_Interrupt_Indicated return Boolean
     with Inline;
   --  Set by hardware when the counter has reached the value 16#40#, the
   --  counter value just prior to the reset being triggered. Always set,
   --  even if the interrupt is not enabled.

   procedure Clear_Early_Wakeup_Interrupt
     with Inline;
   --  Clears the status bit

   function WWDG_Reset_Indicated return Boolean;
   --  Indicated in the RCC peripheral

   procedure Clear_WWDG_Reset_Flag;
   --  In the RCC peripheral

   procedure Reset_System;
   --  an immediate software-driven reset, just as if the count hit 16#3F#

end STM32.WWDG;
