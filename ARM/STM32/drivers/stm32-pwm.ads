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

--  This file provides a convenient pulse width modulation (PWM) generation
--  abstract data type.

--  Example use, with arbitrary hardware selections:

--     PWM_Timer : STM32.Timers.Timer renames Timer_4;
--
--     Requested_Frequency : constant Hertz := 30_000;  -- arbitrary
--
--     Power_Control : PWM_Modulator;
--
--     ...
--
--     Initialize_PWM_Modulator
--       (Power_Control,
--        Generator => PWM_Timer'Access,
--        Frequency => Requested_Frequency,
--        Configure_Generator => True);
--
--     Attach_PWM_Channel
--       (Power_Control,
--        Output_Channel,
--        LED_For (Output_Channel),
--        GPIO_AF_2_TIM4);
--
--     Enable_PWM (Power_Control);
--
--     Set_Duty_Cycle (Power_Control, Value);

with STM32.GPIO;   use STM32.GPIO;
with STM32.Timers; use STM32.Timers;

package STM32.PWM is
   pragma Elaborate_Body;

   type PWM_Modulator is limited private;

   subtype Hertz is UInt32;

   procedure Initialize_PWM_Modulator
     (This                : in out PWM_Modulator;
      Generator           : not null access STM32.Timers.Timer;
      Frequency           : Hertz;
      Configure_Generator : Boolean := True);
   --  Initializes the PWM modulator. To be called at least once per
   --  PWM_Modulator object, before any other use of the given modulator. Can
   --  be called more than once in case the user wants to tie the modulator to
   --  a different timer.
   --
   --  Frequency is that value intended for the underlying timer. To be
   --  specified regardless of whether Configure_Generator is True.
   --
   --  If Configure_Generator is True, the Generator timer hardware will be
   --  configured for PWM generation with the specified frequency. For any
   --  given timer, this configuration need only be done once, unless a
   --  different frequency is required for some reason.

   procedure Attach_PWM_Channel
     (This    : in out PWM_Modulator;
      Channel : Timer_Channel;
      Point   : GPIO_Point;
      PWM_AF  : GPIO_Alternate_Function)
     with Post => not Enabled (This) and
                  Current_Duty_Cycle (This) = 0;
   --  Initializes the channel on the timer associated with This modulator,
   --  and the corresponding GPIO port/pin pair, for PWM output.
   --
   --  To be called at least once. May Be called multiple times for the same
   --  PWM_Modulator object, with different channels, because the corresponding
   --  timer can drive multiple channels (assuming such a timer is in use).

   procedure Enable_PWM
     (This : in out PWM_Modulator)
     with Post => Enabled (This);

   function Enabled (This : PWM_Modulator) return Boolean;

   procedure Disable_PWM
     (This : in out PWM_Modulator)
     with Post => not Enabled (This);

   subtype Percentage is Integer range 0 .. 100;

   procedure Set_Duty_Cycle
     (This  : in out PWM_Modulator;
      Value : Percentage)
     with
       Inline,
       Post => Current_Duty_Cycle (This) = Value;
   --  Sets the pulse width such that the requested percentage is achieved.

   function Current_Duty_Cycle (This : PWM_Modulator) return Percentage
     with Inline;

   subtype Microseconds is UInt32;

   procedure Set_Duty_Time
     (This  : in out PWM_Modulator;
      Value : Microseconds)
     with
       Inline;
   --  Set the pulse width such that the requested number of microseconds is
   --  achieved. Raises Invalid_Request if the requested time is greater than
   --  the period previously configured via Configure_PWM_Timer.

   Invalid_Request : exception;
   --  Raised when the requested frequency is too high or too low for the given
   --  timer and system clocks when calling Configure_PWM_Timer, or when
   --  the requested time is too high for the specified frequency when calling
   --  Set_Duty_Time

   Unknown_Timer : exception;
   --  Raised if a timer that is not known to the package is passed to
   --  Configure_PWM_Timer

private

   type PWM_Modulator is record
      Generator    : access STM32.Timers.Timer;
      Timer_Period : UInt32;
      Frequency    : UInt32;
      Duty_Cycle   : Percentage := 0;
      Channel      : Timer_Channel;
   end record;

   procedure Configure_PWM_Timer
     (This                : not null access STM32.Timers.Timer;
      Requested_Frequency : Hertz;
      Computed_Period     : out UInt32);
   --  Configures the specified timer hardware for PWM generation at the
   --  requested frequency. Computes the period required for the requested
   --  frequency. This is a separate procedure, distinct from the
   --  initialization for objects of type PWM_Modulator, because a timer can
   --  be used by several modulator objects.

end STM32.PWM;
