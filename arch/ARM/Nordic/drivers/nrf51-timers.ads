------------------------------------------------------------------------------
--                                                                          --
--                       Copyright (C) 2017, AdaCore                        --
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

with NRF51_SVD.TIMER;
with HAL;             use HAL;

package nRF51.Timers is

   type Timer (Periph : not null access NRF51_SVD.TIMER.TIMER_Peripheral)
   is tagged private;

   type Timer_Channel is range 0 .. 3;

   -------------------
   -- Configuration --
   -------------------

   procedure Set_Prescaler (This      : in out Timer;
                            Prescaler : UInt4);
   --  Set timer prescaler value
   --
   --  Timer frequency = HFCLK / (2**Prescaler)
   --
   --  This procedure should only be called when the timer is stopped.

   type Timer_Mode is (Mode_Timer, Mode_Counter);

   procedure Set_Mode (This : in out Timer;
                       Mode : Timer_Mode);
   --  Set timer mode:
   --   - Mode_Counter: Timer internal count register is incremented everytime
   --     the count task is triggered.
   --   - Mode_Timer: Timer internal count register is incremented for every
   --     tick of the timer clock. The frequency of the timer clock depends on
   --     the prescaler value (see procedure Set_Prescaler).

   type Timer_Bitmode is (Bitmode_8bit,
                          Bitmode_16bit,
                          Bitmode_24bit,
                          Bitmode_32bit);

   procedure Set_Bitmode (This : in out Timer;
                          Mode : Timer_Bitmode)
     with Post => Bitmode (This) = Mode;
   --  Set bit width of the timer internal count register.
   --
   --  This procedure should only be called when the timer is stopped.

   function Bitmode (This : Timer) return Timer_Bitmode;

   procedure Compare_Interrupt (This   : in out Timer;
                                Chan   : Timer_Channel;
                                Enable : Boolean);
   --  Enable timer interupt for given compare channel

   -------------
   -- Control --
   -------------

   procedure Start (This : in out Timer);
   --  Start the timer

   procedure Stop (This : in out Timer);
   --  Stop the timer

   procedure Clear (This : in out Timer);
   --  Clear timer internal counter

   ---------------
   -- Shortcuts --
   ---------------

   procedure Compare_Shortcut (This  : in out Timer;
                               Chan  : Timer_Channel;
                               Stop  : Boolean;
                               Clear : Boolean);
   --  Clear : clear timer internal counter when compare event is triggered for
   --  given channel.
   --  Stop : stop the timer when compare envet N is triggered for given channel


   -----------------------
   -- Capture / Compare --
   -----------------------

   procedure Set_Compare (This    : in out Timer;
                          Chan    : Timer_Channel;
                          Compare : UInt32);
   --  Set compare/capture register of given channel


   procedure Capture (This : in out Timer;
                      Chan : Timer_Channel);
   --  Capture value of timer internal counter in the compare/capture register
   --  of given channel.

   function Capture (This : in out Timer;
                     Chan : Timer_Channel)
                     return UInt32;
   --  Capture value of timer internal counter in the compare/capture register
   --  of given channel and return the value of this register.

   function CC_Register (This : in out Timer;
                         Chan : Timer_Channel)
                         return UInt32;
   --  Return the value of compare/capture register of given channel

   ----------------------
   -- Tasks and Events --
   ----------------------

   function Start_Task (This : Timer) return Task_Type;
   function Stop_Task  (This : Timer) return Task_Type;
   function Count_Task (This : Timer) return Task_Type;
   function Clear_Task (This : Timer) return Task_Type;

   function Capture_Task (This : Timer;
                          Chan : Timer_Channel)
                          return Task_Type;

   function Compare_Event (This : Timer;
                           Chan : Timer_Channel)
                           return Event_Type;

private

   type Timer (Periph : not null access NRF51_SVD.TIMER.TIMER_Peripheral)
   is tagged null record;

end nRF51.Timers;
