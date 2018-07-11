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

with nRF51.Clock;
with nRF51.Device;        use nRF51.Device;
with nRF51.RTC;           use nRF51.RTC;
with nRF51.Events;
with nRF51.Interrupts;
with System.Machine_Code; use System.Machine_Code;

package body MicroBit.Time is

   package Clocks renames nRF51.Clock;

   Clock_Ms  : Time_Ms := 0 with Volatile;
   Period_Ms : constant Time_Ms := 1;

   Subscribers : array (1 .. 10) of Tick_Callback := (others => null);

   procedure Initialize;
   procedure Update_Clock;
   procedure RTC1_IRQHandler;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      if not Clocks.Low_Freq_Running then
         Clocks.Set_Low_Freq_Source (Clocks.LFCLK_XTAL);
         Clocks.Start_Low_Freq;

         loop
            exit when Clocks.Low_Freq_Running;
         end loop;
      end if;

      Stop (RTC_1);

      --  1kHz
      Set_Prescaler (RTC_1, 0);
      Set_Compare (RTC_1, 0, 32);

      Enable_Event (RTC_1, Compare_0_Event);

      nRF51.Events.Enable_Interrupt (nRF51.Events.RTC_1_COMPARE_0);

      nRF51.Interrupts.Register (nRF51.Interrupts.RTC1_Interrupt,
                                 RTC1_IRQHandler'Access);

      nRF51.Interrupts.Enable (nRF51.Interrupts.RTC1_Interrupt);

      Start (RTC_1);
   end Initialize;

   ------------------
   -- Update_Clock --
   ------------------

   procedure Update_Clock is
   begin
      Clock_Ms := Clock_Ms + Period_Ms;
   end Update_Clock;

   ---------------------
   -- RTC1_IRQHandler --
   ---------------------

   procedure RTC1_IRQHandler is
   begin
      Stop (RTC_1);
      Clear (RTC_1);
      Start (RTC_1);

      nRF51.Events.Clear (nRF51.Events.RTC_1_COMPARE_0);

      Update_Clock;

      for Subs of Subscribers loop

         if Subs /= null then
            --  Call the subscriber
            Subs.all;
         end if;

      end loop;
   end RTC1_IRQHandler;

   -----------
   -- Clock --
   -----------

   function Clock return Time_Ms is
   begin
      return Clock_Ms;
   end Clock;

   --------------
   -- Delay_Ms --
   --------------

   procedure Delay_Ms (Milliseconds : UInt64) is
      Wakeup_Time : constant UInt64 := Clock + Milliseconds;
   begin
      while Wakeup_Time > Clock loop
         Asm (Template => "wfi", -- Wait for interrupt
              Volatile => True);
      end loop;
   end Delay_Ms;

   -----------------
   -- Tick_Period --
   -----------------

   function Tick_Period return Time_Ms is
   begin
      return Period_Ms;
   end Tick_Period;

   --------------------
   -- Tick_Subscribe --
   --------------------

   function Tick_Subscriber (Callback : not null Tick_Callback) return Boolean is
   begin
      for Subs of Subscribers loop
         if Subs = Callback then
            return True;
         end if;
      end loop;
      return False;
   end Tick_Subscriber;

   --------------------
   -- Tick_Subscribe --
   --------------------

   function Tick_Subscribe (Callback : not null Tick_Callback) return Boolean is
   begin
      for Subs of Subscribers loop
         if Subs = null then
            Subs := Callback;
            return True;
         end if;
      end loop;

      return False;
   end Tick_Subscribe;

   ----------------------
   -- Tick_Unsubscribe --
   ----------------------

   function Tick_Unsubscribe (Callback : not null Tick_Callback) return Boolean is
   begin
      for Subs of Subscribers loop
         if Subs = Callback then
            Subs := null;
            return True;
         end if;
      end loop;
      return False;
   end Tick_Unsubscribe;

   ---------------
   -- HAL_Delay --
   ---------------

   Delay_Instance : aliased MB_Delays;

   function HAL_Delay return not null HAL.Time.Any_Delays is
   begin
      return Delay_Instance'Access;
   end HAL_Delay;

   ------------------------
   -- Delay_Microseconds --
   ------------------------

   overriding
   procedure Delay_Microseconds (This : in out MB_Delays;
                                 Us   : Integer)
   is
      pragma Unreferenced (This);
   begin
      Delay_Ms (UInt64 (Us / 1000));
   end Delay_Microseconds;

   ------------------------
   -- Delay_Milliseconds --
   ------------------------

   overriding
   procedure Delay_Milliseconds (This : in out MB_Delays;
                                 Ms   : Integer)
   is
      pragma Unreferenced (This);
   begin
      Delay_Ms (UInt64 (Ms));
   end Delay_Milliseconds;

   -------------------
   -- Delay_Seconds --
   -------------------

   overriding
   procedure Delay_Seconds (This : in out MB_Delays;
                            S    : Integer)
   is
      pragma Unreferenced (This);
   begin
      Delay_Ms (UInt64 (S * 1000));
   end Delay_Seconds;

begin
   Initialize;
end MicroBit.Time;
