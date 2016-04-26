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

--  This program demonstrates the on-board "independent watchdog" provided by
--  the STM32F4 family.

--  Generally you will have to cycle power to the board before downloading
--  (flashing) this program. Otherwise it will not run after flashing, and may
--  not successfully finish the flash step itself. In this case the on-board
--  Reset button will not suffice.

--  The program first blinks all the LEDs on and off three times to signal
--  initial execution. It then blinks the green LED indefinitely, also
--  resetting the watchdog so that the board is not reset. If you see all the
--  LEDs blink after initial power-up you know that the board has been reset
--  by the watchdog. That makes it easy to experiment with the watchdog's
--  parameters.

with Last_Chance_Handler;    pragma Unreferenced (Last_Chance_Handler);

with STM32.IWDG;    use STM32.IWDG;
with Ada.Real_Time; use Ada.Real_Time;
with STM32.Board;   use STM32.Board;

procedure Demo_IWDG is
begin
   Initialize_LEDs;

   Initialize_Watchdog (Prescaler => Divider_32, Count => 4095);
   --  The clock driving the watchdog downcounter is approximately 32KHz, so
   --  dividing by 32 gives about 1 millisecond per count. Thus an initial
   --  (and reloaded) count of 4095 gives approximately that many milliseconds
   --  (i.e., about 4.1 seconds) before the system reset interrupt is
   --  triggered. Feel free to experiment with these values...

   for K in 1 .. 3 loop
      All_LEDs_On;
      delay until Clock + Milliseconds (200);
      All_LEDs_Off;
      delay until Clock + Milliseconds (200);
   end loop;

   Start_Watchdog;

   loop
      Turn_On (Green);
      delay until Clock + Milliseconds (500);
      Turn_Off (Green);
      delay until Clock + Milliseconds (500);

      Reset_Watchdog;
   end loop;
end Demo_IWDG;
