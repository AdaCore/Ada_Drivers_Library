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

--  This demonstration illustrates the use of PWM to control the brightness of
--  an LED. The effect is to make the LED increase and decrease in brightness,
--  iteratively, for as long as the application runs. In effect the LED light
--  waxes and wanes. See http://visualgdb.com/tutorials/arm/stm32/fpu/ for the
--  inspiration.

with Last_Chance_Handler;  pragma Unreferenced (Last_Chance_Handler);

with STM32F4_Discovery; use STM32F4_Discovery;
--  In this specific case we are using one of the four LEDs on the F4_Disco
--  board, which is tied to a specific timer and channel on that board. As such
--  it would be possible to use a different board, but inconvenient. The PWM
--  package itself is independent of the boards.

with STM32F4.PWM;    use STM32F4.PWM;
with STM32F4.GPIO;   use STM32F4.GPIO;
with STM32F4.Timers; use STM32F4.Timers;
with STM32F4.RCC;    use STM32F4.RCC;

procedure Demo is

   Output : PWM_Modulator;

   Output_Timer : Timer renames Timer_4;

   Output_Channel : constant Timer_Channel := Channel_2;
   --  The LED driven by this example is determined by the channel selected.
   --  That is so because each channel of Timer_4 is connected to a specific
   --  LED in the alternate function configuration on this board. We will
   --  initialize one of the LEDs to be in the AF mode for Timer_4. The
   --  particular channel selected is completely arbitrary, as long as the
   --  selected GPIO port/pin for the LED matches the selected channel.

   --  Channel_1 is connected to the green LED.
   --  Channel_2 is connected to the orange LED.
   --  Channel_3 is connected to the red LED.
   --  Channel_4 is connected to the blue LED.

   Output_Point : constant GPIO_Point := (LED_Port'Access, Orange);
   --  This must match the GPIO port/pin for the selected Output_Channel value.

   --  The SFP run-time library for these boards is intended for certified
   --  environments and so does not contain the full set of facilities defined
   --  by the Ada language. The elementary functions are not included, for
   --  example. In contrast, the Ravenscar "full" run-times do have these
   --  functions.

   --  Therefore there are four choices: 1) use the "ravescar-full-stm32f4"
   --  runtime library, 2) pull the sources for the language-defined elementary
   --  function package into the board's run-time library and rebuild the
   --  run-time, 3) pull the sources for those packages into the source
   --  directory of your application and rebuild your application, or 4) roll
   --  your own approximation to the functions required by your application.

   --  In this demonstration we roll our own approximation to the sine function
   --  so that it doesn't matter which runtime library is used.

      function Sine (Input : Long_Float) return Long_Float is
         Pi : constant Long_Float := 3.14159_26535_89793_23846;
         X  : constant Long_Float := Long_Float'Remainder(Input, Pi * 2.0);
         B  : constant Long_Float := 4.0 / Pi;
         C  : constant Long_Float := (-4.0) / (Pi * Pi);
         Y  : constant Long_Float := B * X + C * X * abs (X);
         P  : constant Long_Float := 0.225;
      begin
         return P * (Y * abs (Y) - Y) + Y;
      end Sine;

   --  We use the sine function to drive the power applied to the LED, thereby
   --  making the LED increase and decrease in brightness. We attach the timer
   --  to the LED and then control how much power is supplied by changing the
   --  value of the timer's output compare register. The sine function drives
   --  that value, thus the waxing/waning effect.

begin
   Initialise_PWM_Modulator
     (Output,
      Requested_Frequency    => 30_000.0, -- arbitrary
      PWM_Timer              => Output_Timer'Access,
      PWM_AF                 => GPIO_AF_TIM4,
      Enable_PWM_Timer_Clock => TIM4_Clock_Enable'Access);

   Attach_PWM_Channel
     (Output,
      Output_Channel,
      Output_Point,
      GPIOD_Clock_Enable'Access);

   declare
      use STM32F4;
      Arg       : Long_Float := 0.0;
      Value     : Percentage;
      Increment : constant Long_Float := 0.00003;
      --  The Increment value controls the rate at which the brightness
      --  increases and decreases. The value is more or less arbitrary, but
      --  note that the effect of optimization is observable.
   begin
      loop
         Value := Percentage (50.0 * (1.0 + Sine (Arg)));
         Set_Duty_Cycle (Output, Output_Channel, Value);
         Arg := Arg + Increment;
      end loop;
   end;
end Demo;
