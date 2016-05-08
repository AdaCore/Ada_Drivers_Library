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

   Watchdog_Count : constant := 4095;  -- arbitrary
   --  The clock driving the watchdog downcounter is approximately 32KHz, so
   --  dividing by 32 gives about 1 millisecond per count. Thus the count gives
   --  approximately that many milliseconds before the system reset interrupt
   --  is triggered. Feel free to experiment with this value.

   Watchdog_Count_Offset : constant := -90; -- arbitrary
   --  The entirely arbitrary amount to add/subtract to the watchdog count, ie
   --  the time that causes a board reset, in order to experiment with causing
   --  the reset to occur. This value is added to the Watchdog_Count and the
   --  result passed to Do_Work, thereby controlling whether the watchdog timer
   --  trips or not. Feel free to experiment with this value.

   Work_Time : constant := Watchdog_Count + Watchdog_Count_Offset;
   --  The value passed to Do_Work which determines the amount of time taken by
   --  that call. By changing the value of Watchdog_Count_Offset you can cause
   --  the reset to occur, or not.  Note that optimization will have influence.

   procedure Do_Work (Time_Required : Time_Span);
   --  A simple abstraction for some amount of work to be done while the
   --  watchdog is running. The Time_Required parameter controls how long the
   --  "work" should take, so that you can experiment with causing the reset
   --  to occur by giving it a value close to the watchdog timeout interval.

   procedure Do_Work (Time_Required : Time_Span) is
      Interval : constant Time_Span := Time_Required / 4;
      --  We divide so that we can blink more often
   begin
      Turn_On (Green);
      delay until Clock + Interval;
      Turn_Off (Green);
      delay until Clock + Interval;
      Turn_On (Green);
      delay until Clock + Interval;
      Turn_Off (Green);
      delay until Clock + Interval;
   end Do_Work;

begin
   Initialize_LEDs;

   Initialize_Watchdog (Prescaler => Divider_32, Count => Watchdog_Count);

   --  Indicate initial excution, but also indicate board (program) reset
   for K in 1 .. 3 loop
      All_LEDs_On;
      delay until Clock + Milliseconds (200);
      All_LEDs_Off;
      delay until Clock + Milliseconds (200);
   end loop;

   Start_Watchdog;
   loop
      Do_Work (Time_Required => Milliseconds (Work_Time));
      --  If Do_Work does not return in time so that the call to reset the
      --  watchdog can occur, the board will reset and the program will
      --  restart.

      Reset_Watchdog;
   end loop;
end Demo_IWDG;
