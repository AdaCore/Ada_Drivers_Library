------------------------------------------------------------------------------
--                                                                          --
--                        Copyright (C) 2019, AdaCore                       --
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
--     3. Neither the name of the copyright holder nor the names of its     --
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


with SAM.Clock_Setup_120Mhz;
with SAM.Device;
with SAM.Port;
with HAL.GPIO;

procedure Main is

   L_LED  : SAM.Port.GPIO_Point renames SAM.Device.PA16;
   TX_LED : SAM.Port.GPIO_Point renames SAM.Device.PA27;
   RX_LED : SAM.Port.GPIO_Point renames SAM.Device.PB06;

   Dont_Optim : Integer with Volatile, Unreferenced;
begin

   SAM.Clock_Setup_120Mhz.Initialize_Clocks;

   L_LED.Set_Mode (HAL.GPIO.Output);
   TX_LED.Set_Mode (HAL.GPIO.Output);
   RX_LED.Set_Mode (HAL.GPIO.Output);

   L_LED.Clear;
   TX_LED.Set;
   RX_LED.Set;

   loop
      L_LED.Toggle;
      TX_LED.Toggle;
      RX_LED.Toggle;

      for X in 1 .. 1_000_000 loop
         --  Busy loop...
         Dont_Optim := X;
      end loop;
   end loop;
end Main;
