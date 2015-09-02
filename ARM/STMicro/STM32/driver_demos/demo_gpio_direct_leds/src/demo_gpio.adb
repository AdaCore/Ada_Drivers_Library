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

--  A simple example that blinks all the LEDs simultaneously, w/o tasking. It
--  does not use the "board" packages and so works directly with the platform
--  package STM32F40xx and the GPIO driver to control the LEDs.

--  Note that this is set up for an STM32F4_Discovery board because it uses
--  four LEDs, but it would be trivial to change it to another board. The
--  F4_Disco board is based on an STM32F405 MCU so we use the STM32F40xxx
--  package to get the GPIO port GPIO_D for the LEDs.
--
--  Note that using the STM32F4_Discovery package would make this even easier
--  but that is not what this program demonstrates.

with STM32F40xxx;   use STM32F40xxx;

with STM32F4.GPIO;  use STM32F4.GPIO;
with Ada.Real_Time; use Ada.Real_Time;
with STM32F4.RCC;

procedure Demo_GPIO is

   use STM32F4;

   All_LEDs : constant GPIO_Pins := Pin_12 & Pin_13 & Pin_14 & Pin_15;

   Period : constant Time_Span := Milliseconds (250);
   Next   : Time := Clock;

   Enabled : Boolean := False;

   procedure Initialize_LEDs is
      Configuration : GPIO_Port_Configuration;
   begin
      RCC.GPIOD_Clock_Enable;

      Configuration.Mode        := Mode_Out;
      Configuration.Output_Type := Push_Pull;
      Configuration.Speed       := Speed_100MHz;
      Configuration.Resistors   := Floating;
      Configure_IO (Port => GPIO_D, Pins => All_LEDs, Config => Configuration);
   end Initialize_LEDs;

begin
   Initialize_LEDs;

   loop
      if Enabled then
         Set (GPIO_D, All_LEDs);
      else
         Clear (GPIO_D, All_LEDs);
      end if;
      Enabled := not Enabled;
      Next := Next + Period;
      delay until Next;
   end loop;
end Demo_GPIO;
