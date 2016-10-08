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

--  This file provides a convenient pulse width modulation (PWM) generation
--  abstract data type.

--  Example use, with arbitrary hardware selections:

--     Timer  : aliased PWM_Timer (Timer_4'Access);
--     Output : PWM_Modulator;
--
--     Initialize_PWM_Timer
--       (Timer,
--        Requested_Frequency    => 30_000.0,
--
--     Attach_PWM_Channel
--       (Timer'Access,
--        Output,
--        Channel_2,
--        (GPIO_D'Access, Pin_13),  -- must match selected channel
--        PWM_AF                 => GPIO_AF_TIM4);
--
--     Enable_PWM_Channel (Output, Channel_2);
--
--     Set_Duty_Cycle (Output, Channel_2, Value);

with STM32.GPIO;   use STM32.GPIO;
with STM32.Timers; use STM32.Timers;

package STM32.PWM is
   pragma Elaborate_Body;

   type PWM_Timer
     (Output_Timer : not null access STM32.Timers.Timer) is limited private;

   procedure Initialise_PWM_Timer
     (This                   : in out PWM_Timer;
      Requested_Frequency    : Float);
   --  Initializes the specified timer for PWM generation at the requested
   --  frequency. You must attach at least one channel to the modulator in
   --  order to drive PWM output values.

   type PWM_Modulator is limited private;

   procedure Attach_PWM_Channel
     (This      : access PWM_Timer;
      Modulator : in out PWM_Modulator;
      Channel   : Timer_Channel;
      Point     : GPIO_Point;
      PWM_AF    : GPIO_Alternate_Function)
     with Post => Attached (Modulator) and
                  not Enabled (Modulator) and
                  Current_Duty_Cycle (Modulator) = 0;
   --  Initializes the channel on the timer associated with This, and the
   --  corresponding GPIO port/pin pair, for PWM output. May be called multiple
   --  times for the same PWM_Modulator object, with different channels,
   --  because the corresponding timer can drive multiple channels (assuming
   --  such a timer is in use).

   procedure Enable_PWM
     (This : in out PWM_Modulator)
     with Post => Enabled (This);

   function Enabled
     (This : PWM_Modulator)
      return Boolean;

   procedure Disable_PWM
     (This : in out PWM_Modulator)
     with Post => not Enabled (This);

   subtype Percentage is Integer range 0 .. 100;

   procedure Set_Duty_Cycle
     (This  : in out PWM_Modulator;
      Value : Percentage)
     with
       Inline,
       Pre  => (Attached (This) or else raise Not_Attached),
       Post => Current_Duty_Cycle (This) = Value;
   --  Sets the pulse width such that the requested percentage is achieved.

   function Current_Duty_Cycle (This : PWM_Modulator) return Percentage
     with Inline,
          Pre => Attached (This) or else raise Not_Attached;

   subtype Microseconds is UInt32;

   procedure Set_Duty_Time
     (This  : in out PWM_Modulator;
      Value : Microseconds)
     with
       Inline,
       Pre => (Attached (This) or else raise Not_Attached);
   --  Set the pulse width such that the requested number of microseconds is
   --  achieved. Raises Invalid_Request if the requested time is greater than
   --  the period previously configured via Initialise_PWM_Modulator.

   Invalid_Request : exception;
   --  Raised when the requested frequency is too high or too low for the given
   --  timer and system clocks when calling Initialize_PWM_Modulator, or when
   --  the requested time is too high for the specified frequency when calling
   --  Set_Duty_Time

   Unknown_Timer : exception;
   --  Raised if a timer that is not physically present is passed to
   --  Initialize_PWM_Modulator

   Not_Attached : exception;

   function Attached
     (This    : PWM_Modulator)
      return Boolean;

private

   type PWM_Output is record
      Duty_Cycle : Percentage := 0;
      Attached   : Boolean := False;
   end record;

   type PWM_Outputs is array (Timer_Channel) of PWM_Output;

   type PWM_Timer
     (Output_Timer : not null access STM32.Timers.Timer)
   is record
      Outputs      : PWM_Outputs;
      Timer_Period : UInt32;
      Frequency    : UInt32;
   end record;

   type PWM_Modulator is record
      Timer   : access PWM_Timer;
      Channel : Timer_Channel;
   end record;

end STM32.PWM;
