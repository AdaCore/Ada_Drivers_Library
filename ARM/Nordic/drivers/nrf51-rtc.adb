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

with NRF51_SVD.RTC; use NRF51_SVD.RTC;

package body nRF51.RTC is

   -----------
   -- Start --
   -----------

   procedure Start (This : Real_Time_Counter) is
   begin
      This.Periph.TASKS_START := 1;
   end Start;

   ----------
   -- Stop --
   ----------

   procedure Stop (This : Real_Time_Counter) is
   begin
      This.Periph.TASKS_STOP := 1;
   end Stop;

   -----------
   -- Clear --
   -----------

   procedure Clear (This : Real_Time_Counter) is
   begin
      This.Periph.TASKS_CLEAR := 1;
   end Clear;

   -------------------
   -- Set_Prescaler --
   -------------------

   procedure Set_Prescaler
     (This      : Real_Time_Counter;
      Prescaler : UInt12)
   is
   begin
      This.Periph.PRESCALER.PRESCALER := Prescaler;
   end Set_Prescaler;

   -------------
   -- Counter --
   -------------

   function Counter (This : Real_Time_Counter) return UInt24 is
   begin
      return This.Periph.COUNTER.COUNTER;
   end Counter;

   -----------------
   -- Set_Compare --
   -----------------

   procedure Set_Compare
     (This    : Real_Time_Counter;
      Compare : Compare_Channel;
      Value   : UInt24)
   is
   begin
      This.Periph.CC (Integer (Compare)).COMPARE := Value;
   end Set_Compare;

   -----------
   -- Event --
   -----------

   function Event
     (This : Real_Time_Counter;
      Evt  : RTC_Events)
      return Event_Type
   is
   begin
      case Evt is
         when Tick_Event      =>
            return Event_Type (This.Periph.EVENTS_TICK'Address);
         when Overflow_Event  =>
            return Event_Type (This.Periph.EVENTS_OVRFLW'Address);
         when Compare_0_Event =>
            return Event_Type (This.Periph.EVENTS_COMPARE (0)'Address);
         when Compare_1_Event =>
            return Event_Type (This.Periph.EVENTS_COMPARE (1)'Address);
         when Compare_2_Event =>
            return Event_Type (This.Periph.EVENTS_COMPARE (2)'Address);
         when Compare_3_Event =>
            return Event_Type (This.Periph.EVENTS_COMPARE (3)'Address);
      end case;
   end Event;

   ------------------
   -- Enable_Event --
   ------------------

   procedure Enable_Event
     (This : Real_Time_Counter;
      Evt  : RTC_Events)
   is
   begin
      case Evt is
         when Tick_Event      =>
            This.Periph.EVTEN.TICK := Enabled;
         when Overflow_Event  =>
            This.Periph.EVTEN.OVRFLW := Enabled;
         when Compare_0_Event =>
            This.Periph.EVTEN.COMPARE.Arr (0) := Enabled;
         when Compare_1_Event =>
            This.Periph.EVTEN.COMPARE.Arr (1) := Enabled;
         when Compare_2_Event =>
            This.Periph.EVTEN.COMPARE.Arr (2) := Enabled;
         when Compare_3_Event =>
            This.Periph.EVTEN.COMPARE.Arr (3) := Enabled;
      end case;
   end Enable_Event;

   -------------------
   -- Disable_Event --
   -------------------

   procedure Disable_Event
     (This : Real_Time_Counter;
      Evt  : RTC_Events)
   is
   begin
      case Evt is
         when Tick_Event      =>
            This.Periph.EVTEN.TICK := Disabled;
         when Overflow_Event  =>
            This.Periph.EVTEN.OVRFLW := Disabled;
         when Compare_0_Event =>
            This.Periph.EVTEN.COMPARE.Arr (0) := Disabled;
         when Compare_1_Event =>
            This.Periph.EVTEN.COMPARE.Arr (1) := Disabled;
         when Compare_2_Event =>
            This.Periph.EVTEN.COMPARE.Arr (2) := Disabled;
         when Compare_3_Event =>
            This.Periph.EVTEN.COMPARE.Arr (3) := Disabled;
      end case;
   end Disable_Event;

end nRF51.RTC;
