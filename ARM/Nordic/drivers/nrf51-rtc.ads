------------------------------------------------------------------------------
--                                                                          --
--                       Copyright (C) 2016, AdaCore                        --
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

with NRF51_SVD.RTC;
with HAL; use HAL;

package nRF51.RTC is

   type RTC_Events is (Tick_Event, Overflow_Event, Compare_0_Event,
                      Compare_1_Event, Compare_2_Event, Compare_3_Event);

   type Compare_Channel is range 0 .. 3;

   type Real_Time_Counter (Periph : not null access NRF51_SVD.RTC.RTC_Peripheral) is private;

   procedure Start (This : Real_Time_Counter);
   procedure Stop (This : Real_Time_Counter);
   procedure Clear (This : Real_Time_Counter);

   procedure Set_Prescaler (This      : Real_Time_Counter;
                            Prescaler : UInt12);

   function Counter (This : Real_Time_Counter) return UInt24;

   procedure Set_Compare (This    : Real_Time_Counter;
                          Compare : Compare_Channel;
                          Value   : UInt24);

   function Event (This : Real_Time_Counter;
                   Evt  : RTC_Events) return Event_Type;

   procedure Enable_Event (This : Real_Time_Counter;
                           Evt  : RTC_Events);
   procedure Disable_Event (This : Real_Time_Counter;
                            Evt  : RTC_Events);
private
   type Real_Time_Counter (Periph : not null access NRF51_SVD.RTC.RTC_Peripheral) is null record;
end nRF51.RTC;
