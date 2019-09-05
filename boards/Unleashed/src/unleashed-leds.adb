------------------------------------------------------------------------------
--                                                                          --
--                       Copyright (C) 2019, AdaCore                        --
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

with SiFive.Device;  use SiFive.Device;
with SiFive.PWM;     use SiFive.PWM;

package body Unleashed.LEDs is

   PWM        : PWM_Device renames PWM0;
   PWM_Scale  : constant := 2;
   PWM_Period : constant := Compare_Value'Last;

   ------------
   -- Enable --
   ------------

   procedure Enable is
   begin
      Enable_Continous (PWM);
   end Enable;

   -------------
   -- Disable --
   -------------

   procedure Disable is
   begin
      Disable (PWM);
   end Disable;

   --------------------
   -- Set_Brightness --
   --------------------

   procedure Set_Brightness (Id : LED_Id; Brightness : LED_Brightness) is
   begin
      Set_Compare (PWM, Comparator_ID (Id),
                   Compare_Value (Float (PWM_Period) * Float (Brightness)));
   end Set_Brightness;

begin
   Configure (This          => PWM,
              Scale         => PWM_Scale,
              Sticky        => False,
              Reset_To_Zero => False,
              Deglitch      => True);

   for Id in LED_Id loop
      Set_Brightness (Id, LED_Brightness'Last);
   end loop;

   Set_Count (PWM, 0);
end Unleashed.LEDs;
