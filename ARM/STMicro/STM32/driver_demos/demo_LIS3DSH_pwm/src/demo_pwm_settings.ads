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

with STM32_Board;     use STM32_Board;
with STM32.GPIO;      use STM32.GPIO;
with STM32.Timers;    use STM32.Timers;
with STM32.RCC;       use STM32.RCC;
with STM32.PWM;       use STM32.PWM;

package Demo_PWM_Settings is

   PWM_Output : aliased PWM_Modulator;

   PWM_Output_Timer : Timer renames Timer_4;

   PWM_Output_AF : constant GPIO_Alternate_Function := GPIO_AF_TIM4;

   procedure Output_Timer_Clock_Enable renames TIM4_Clock_Enable;

   PWM_Frequency : constant := 10_000.0;  -- arbitrary

   --  On Tmer 4 the channels are connected as follows:
   --
   --  Channel_1 is connected to the green LED.
   --  Channel_2 is connected to the orange LED.
   --  Channel_3 is connected to the red LED.
   --  Channel_4 is connected to the blue LED.

   Channel_1_Point : constant GPIO_Point := (LED_Port'Access, Green);

   Channel_2_Point : constant GPIO_Point := (LED_Port'Access, Orange);

   Channel_3_Point : constant GPIO_Point := (LED_Port'Access, Red);

   Channel_4_Point : constant GPIO_Point := (LED_Port'Access, Blue);

   procedure Channel_GPIO_Clock_Enable
     renames GPIOD_Clock_Enable;
   --  the LEDs on the F4_Disco are all on GPIO_D, which is also the value of
   --  LED_Port

end Demo_PWM_Settings;
