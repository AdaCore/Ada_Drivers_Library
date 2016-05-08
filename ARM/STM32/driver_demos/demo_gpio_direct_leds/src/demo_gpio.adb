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

--  A simple example that blinks all the LEDs simultaneously, w/o tasking.
--  It does not use the various convenience functions defined elsewhere, but
--  instead works directly with the GPIO driver to configure and control the
--  LEDs.

--  Note that this code is independent of the specific MCU device and board
--  in use because we use names and constants that are common across all of
--  them. For example, "All_LEDs" refers to different GPIO pins on different
--  boards, and indeed defines a different number of LEDs on different boards.
--  The gpr file determines which board is actually used.

with STM32.Device;  use STM32.Device;
with STM32.Board;   use STM32.Board;

with STM32.GPIO;    use STM32.GPIO;
with Ada.Real_Time; use Ada.Real_Time;

procedure Demo_GPIO is

   use STM32;

   Period : constant Time_Span := Milliseconds (250);
   Next   : Time := Clock;

   procedure Initialize_LEDs;
   --  Configures the GPIO pins and port connected to the LEDs on the board
   --  in use so that we can drive them via GPIO commands. Note that the board
   --  package provides a procedure to do this directly, for convenience, but
   --  we do not use it here for the sake of illustration.

   procedure Initialize_LEDs is
      Configuration : GPIO_Port_Configuration;
   begin
      Enable_Clock (All_LEDs);

      Configuration.Mode        := Mode_Out;
      Configuration.Output_Type := Push_Pull;
      Configuration.Speed       := Speed_100MHz;
      Configuration.Resistors   := Floating;
      Configure_IO (All_LEDs, Configuration);
   end Initialize_LEDs;

begin
   Initialize_LEDs;

   loop
      Toggle (All_LEDs);

      Next := Next + Period;
      delay until Next;
   end loop;
end Demo_GPIO;
