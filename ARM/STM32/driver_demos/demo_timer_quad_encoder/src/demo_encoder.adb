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

--  Thius program demonstrates the encoder interface provided by the ST Micro
--  timers. We use a timer (TIM3) to emulate quadrature encoder hardware as if
--  attached to a motor, providing the two off-phase signals a hardware encoder
--  would produce. These two signals are fed into another timer (TIM1) working
--  in the encoder interface mode. It is this encoder interface mode that we
--  are demonstrating. The emulated encoder hardware outputs are connected to
--  the encoder interface using jumper wires connecting the corresponding pins
--  indicated by the diagram below:

--      Emulated_Encoder                               Interface
--       -------------                               -------------
--      |             |CH1:PC6              CH1:PA8 |             |
--      |             |---------------------------->|             |
--      |    TIM3     |                             |    TIM1     |
--      |             |CH2:PC7             CH2:PE11 |             |
--      | Quadrature  |---------------------------->| Quadrature  |
--      | encoder     |                             | encoder     |
--      | emulator    |                             | interface   |
--      |             |                             |             |
--      |             |                             |             |
--       -------------                               -------------

--  Timer channels 1 and 2 are used on both sides, and are thus connected to
--  the indicated GPIO pins.

--  When run, the green and red LEDs will alternate blinking at one-second
--  intervals as the emulated encoder is "driven" forward and backward at
--  that rate. The red LED indicates backward, the green forward.

--  This demonstration should work on any STM32 board (that has two LEDs), and
--  is known to work on F4 Discovery and F429 Discovery boards.

--  Based on a demonstration provided by ST Micro:
--  C:\STM32Cube_FW_F4_V1.6.0\Projects\STM324x9I_EVAL\Examples\TIM\TIM_Encoder

with Last_Chance_Handler;  pragma Unreferenced (Last_Chance_Handler);
with Ada.Real_Time;        use Ada.Real_Time;

with STM32.Board;          use STM32.Board;

with Motor;
with Encoder_Emulator;

procedure Demo_Encoder is

   procedure Indicate_Direction;
   --  show the user the currently driven direction of the emulated quad motor
   --  encoder

   procedure Indicate_Direction is
   begin
      case Motor.Current_Direction is
         when Motor.Forward =>
            Turn_Off (Red);
            Turn_On (Green);
         when Motor.Backward =>
            Turn_Off (Green);
            Turn_On (Red);
      end case;
   end Indicate_Direction;

begin
   Initialize_LEDs;

   loop
      Encoder_Emulator.Stop;
      Encoder_Emulator.Emulate_Forward_Direction;
      Encoder_Emulator.Start;

      delay until Clock + Seconds (1);  -- arbitrary

      Indicate_Direction;

      Encoder_Emulator.Stop;
      Encoder_Emulator.Emulate_Backward_Direction;
      Encoder_Emulator.Start;

      delay until Clock + Seconds (1);  -- arbitrary

      Indicate_Direction;
   end loop;
end Demo_Encoder;
