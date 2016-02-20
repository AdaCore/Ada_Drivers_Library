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

--  Emulate a quadrature encoder's outputs

--       -------------
--      |             |CH1:PC6
--      |             |------------->
--      |    TIM3     |
--      |             |CH2:PC7
--      | Quadrature  |------------->
--      | encoder     |
--      | emulator    |
--      |             |
--      |             |
--       -------------

--  Sample calls:
--        Encoder_Emulator.Stop;
--        Encoder_Emulator.Emulate_Forward_Direction;
--        Encoder_Emulator.Start;

with Interfaces;    use Interfaces;

with STM32.Device;  use STM32.Device;
with STM32.GPIO;    use STM32.GPIO;
with STM32.Timers;  use STM32.Timers;
with STM32;         use STM32;

package Encoder_Emulator is
   pragma Elaborate_Body;

   procedure Start;
   --  the first time this is called, the direction will be forward

   procedure Stop;

   procedure Emulate_Forward_Direction;

   procedure Emulate_Backward_Direction;

private

   Emulator_Port  : GPIO_Port renames GPIO_C;

   Emulator_Pins  : constant GPIO_Pins := Pin_6 & Pin_7;

   Emulator_Timer : Timer renames Timer_3;

   Emulator_AF    : constant GPIO_Alternate_Function := GPIO_AF_TIM3;

   Emulator_Period : constant Word := ((System_Clock_Frequencies.SYSCLK / 4) / 10000) - 1;

end Encoder_Emulator;
