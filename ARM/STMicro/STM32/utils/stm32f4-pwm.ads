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

--  Example use:

--     PWM : PWM_Modulator;
--
--     Initialise_PWM_Modulator
--       (PWM,
--        Frequency              => 45_000.0, -- arbitrary
--        PWM_Timer              => Timer_4'Access,
--        PWM_AF                 => GPIO_AF_TIM4,
--        Output                 => (GPIO_D'Access, Pin_12),
--        PWM_Output_Channel     => Channel_1,
--        Enable_PWM_Port_Clock  => GPIOD_Clock_Enable'Access,
--        Enable_PWM_Timer_Clock => TIM4_Clock_Enable'Access);
--
--     declare
--        Value : Percentage := 0;
--     begin
--        loop
--           for K in 0 .. 10 loop
--              Value := K * 10;
--              Print (Value);
--              Set_Duty_Cycle (PWM, Value);
--              delay until Clock + Seconds (3);
--           end loop;
--        end loop;
--     end;

with STM32F4.GPIO;   use STM32F4.GPIO;
with STM32F4.Timers; use STM32F4.Timers;

package STM32F4.PWM is
   pragma Elaborate_Body;

   type PWM_Modulator is limited private;

   subtype Percentage is Integer range 0 .. 100;

   procedure Set_Duty_Cycle (This : in out PWM_Modulator;  Value : Percentage);
   --  Sets the pulse width such that the requested percentage is achieved.

   function Current_Duty_Cycle (This : PWM_Modulator) return Percentage
     with Inline;

   subtype Microseconds is Word;

   procedure Set_Duty_Time (This : in out PWM_Modulator; Value : Microseconds);
   --  Set the pulse width such that the requested number of microseconds is
   --  achieved. Raises Invalid_Request if the requested time is greater than
   --  the previously configured period.

   procedure Initialise_PWM_Modulator
     (This                   : in out PWM_Modulator;
      Requested_Frequency    : Float;
      PWM_Timer              : not null access Timer;
      PWM_AF                 : GPIO_Alternate_Function;
      Output                 : GPIO_Point;
      PWM_Output_Channel     : Timer_Channel;
      Enable_PWM_Port_Clock  : not null access procedure;
      Enable_PWM_Timer_Clock : not null access procedure)
     with Post => Current_Duty_Cycle (This) = 0;

   Invalid_Request : exception;
   --  Raised when the requested frequency is too high or too low for the given
   --  timer and system clocks when calling Initialize_PWM_Modulator, or when
   --  the requested time is too high for the specified frequency when calling
   --  Set_Duty_Time

   Unknown_Timer : exception;

private

   type PWM_Modulator is record
      Duty_Cycle     : Percentage := 0;
      Output_Timer   : access Timer;
      Output_Channel : Timer_Channel;
      Timer_Period   : Word;
      Frequency      : Word;
   end record;

end STM32F4.PWM;
