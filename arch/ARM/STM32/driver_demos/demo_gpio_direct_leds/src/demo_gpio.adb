------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2017, AdaCore                           --
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

--  A simple example that blinks all the LEDs simultaneously, w/o tasking. It
--  does not use the "board" packages and so works directly with the platform
--  package STM32F40xx and the GPIO driver to control the LEDs.

--  Note that this is set up for an STM32F4_Discovery board because it uses
--  four LEDs, but it would be trivial to change it to another board. The
--  F4_Disco board is based on an STM32F405 MCU so we use the STM32F40xxx
--  package to get the GPIO port GPIO_D for the LEDs.
--
--  Note that using the STM32.Board package would make this even easier
--  but that is not what this program demonstrates.

with STM32.User_Button;
with STM32.Device;  use STM32.Device;
with STM32.GPIO;    use STM32.GPIO;
with Ada.Real_Time; use Ada.Real_Time;

procedure Demo_GPIO is

   Green_LED  : GPIO_Point renames PD12;
   Orange_LED : GPIO_Point renames PD13;
   Red_LED    : GPIO_Point renames PD14;
   Blue_LED   : GPIO_Point renames PD15;

   Pattern : GPIO_Points := (Orange_LED, Red_LED, Blue_LED, Green_LED);
   --  The LEDs on the F4 Disco are not physically laid out "consecutively"
   --  in such a way that we can simply go in enumeral order to get circular
   --  rotation. Thus we define this mapping, using a consecutive index to get
   --  the physical LED blinking order desired.

   Next_LED  : Positive range Pattern'Range := Pattern'First;
   Clockwise : Boolean := True;

   Period       : constant Time_Span := Milliseconds (75);  -- arbitrary
   Next_Release : Time := Clock;

   procedure Initialize_LEDs;
   --  Enable the clock for the four LEDs and configure them as outputs. Note
   --  there is a procedure defined in STM32.Board to do this. We do it here
   --  to demonstrate the use of the GPIO interface.

   procedure Initialize_LEDs is
      Configuration : GPIO_Port_Configuration;
   begin
      Enable_Clock (GPIO_D);

      Configuration.Mode        := Mode_Out;
      Configuration.Output_Type := Push_Pull;
      Configuration.Speed       := Speed_100MHz;
      Configuration.Resistors   := Floating;
      Configure_IO (Pattern, Config => Configuration);
   end Initialize_LEDs;

begin
   Initialize_LEDs;
   STM32.User_Button.Initialize;

   loop
      Pattern (Next_LED).Clear;

      if STM32.User_Button.Has_Been_Pressed then
         Clockwise := not Clockwise;
      end if;

      if Clockwise then
         Next_LED := (if Next_LED = Pattern'Last then Pattern'First else Next_LED + 1);
      else
         Next_LED := (if Next_LED = Pattern'First then Pattern'Last else Next_LED - 1);
      end if;

      Pattern (Next_LED).Set;

      Next_Release := Next_Release + Period;
      delay until Next_Release;
   end loop;
end Demo_GPIO;
