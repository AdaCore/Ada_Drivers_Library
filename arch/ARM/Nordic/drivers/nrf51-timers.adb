------------------------------------------------------------------------------
--                                                                          --
--                       Copyright (C) 2017, AdaCore                        --
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

with NRF51_SVD.TIMER; use NRF51_SVD.TIMER;

package body nRF51.Timers is

   -----------
   -- Start --
   -----------

   procedure Start (This : in out Timer) is
   begin
      This.Periph.TASKS_START := 1;
   end Start;

   ----------
   -- Stop --
   ----------

   procedure Stop (This : in out Timer) is
   begin
      This.Periph.TASKS_STOP := 1;
   end Stop;

   -----------
   -- Clear --
   -----------

   procedure Clear (This : in out Timer) is
   begin
      This.Periph.TASKS_CLEAR := 1;
   end Clear;

   -------------------
   -- Set_Prescaler --
   -------------------

   procedure Set_Prescaler
     (This      : in out Timer;
      Prescaler : UInt4)
   is
   begin
      This.Periph.PRESCALER.PRESCALER := Prescaler;
   end Set_Prescaler;

   --------------
   -- Set_Mode --
   --------------

   procedure Set_Mode
     (This : in out Timer;
      Mode : Timer_Mode)
   is
   begin
      This.Periph.MODE.MODE :=
        (case Mode is
            when Mode_Timer => NRF51_SVD.TIMER.Timer,
            when Mode_Counter => NRF51_SVD.TIMER.Counter);
   end Set_Mode;

   -----------------
   -- Set_Bitmode --
   -----------------

   procedure Set_Bitmode
     (This : in out Timer;
      Mode : Timer_Bitmode)
   is
   begin
      This.Periph.BITMODE.BITMODE :=
        (case Mode is
            when Bitmode_8bit  => BITMODE_BITMODE_Field_08Bit,
            when Bitmode_16bit => BITMODE_BITMODE_Field_16Bit,
            when Bitmode_24bit => BITMODE_BITMODE_Field_24Bit,
            when Bitmode_32bit => BITMODE_BITMODE_Field_32Bit);
   end Set_Bitmode;

   -------------
   -- Bitmode --
   -------------

   function Bitmode (This : Timer) return Timer_Bitmode is
   begin
      return
        (case This.Periph.BITMODE.BITMODE is
            when BITMODE_BITMODE_Field_08Bit => Bitmode_8bit,
            when BITMODE_BITMODE_Field_16Bit => Bitmode_16bit,
            when BITMODE_BITMODE_Field_24Bit => Bitmode_24bit,
            when BITMODE_BITMODE_Field_32Bit => Bitmode_32bit);
   end Bitmode;

   -----------------------
   -- Compare_Interrupt --
   -----------------------

   procedure Compare_Interrupt (This   : in out Timer;
                                Chan   : Timer_Channel;
                                Enable : Boolean)
   is
   begin
      if Enable then
         This.Periph.INTENSET.COMPARE.Arr (Integer (Chan)) := Set;
      else
         This.Periph.INTENSET.COMPARE.Arr (Integer (Chan)) := Intenset_Compare0_Field_Reset;
      end if;
   end Compare_Interrupt;

   ----------------------
   -- Compare_Shortcut --
   ----------------------

   procedure Compare_Shortcut (This  : in out Timer;
                               Chan  : Timer_Channel;
                               Stop  : Boolean;
                               Clear : Boolean)
   is
   begin
      case Chan is
         when 0 =>
            This.Periph.SHORTS.COMPARE0_CLEAR := (if Clear then Enabled else Disabled);
            This.Periph.SHORTS.COMPARE0_STOP := (if Stop then Enabled else Disabled);
         when 1 =>
            This.Periph.SHORTS.COMPARE1_CLEAR := (if Clear then Enabled else Disabled);
            This.Periph.SHORTS.COMPARE1_STOP := (if Stop then Enabled else Disabled);
         when 2 =>
            This.Periph.SHORTS.COMPARE2_CLEAR := (if Clear then Enabled else Disabled);
            This.Periph.SHORTS.COMPARE2_STOP := (if Stop then Enabled else Disabled);
         when 3 =>
            This.Periph.SHORTS.COMPARE3_CLEAR := (if Clear then Enabled else Disabled);
            This.Periph.SHORTS.COMPARE3_STOP := (if Stop then Enabled else Disabled);
      end case;
   end Compare_Shortcut;

   -----------------
   -- Set_Compare --
   -----------------

   procedure Set_Compare
     (This    : in out Timer;
      Chan    : Timer_Channel;
      Compare : UInt32)
   is
   begin
      This.Periph.CC (Integer (Chan)) := Compare;
   end Set_Compare;

   -------------
   -- Capture --
   -------------

   procedure Capture (This : in out Timer;
                      Chan : Timer_Channel)
   is
   begin
      This.Periph.TASKS_CAPTURE (Integer (Chan)) := 1;
   end Capture;

   -------------
   -- Capture --
   -------------

   function Capture
     (This : in out Timer;
      Chan : Timer_Channel)
      return UInt32
   is
   begin
      This.Capture (Chan);
      return This.CC_Register (Chan);
   end Capture;

   -----------------
   -- CC_Register --
   -----------------

   function CC_Register (This : in out Timer;
                         Chan : Timer_Channel)
                         return UInt32
   is
   begin
      return This.Periph.CC (Integer (Chan));
   end CC_Register;

   ----------------
   -- Start_Task --
   ----------------

   function Start_Task (This : Timer)
                        return Task_Type
   is (Task_Type (This.Periph.TASKS_START'Address));

   ---------------
   -- Stop_Task --
   ---------------

   function Stop_Task (This : Timer)
                       return Task_Type
   is (Task_Type (This.Periph.TASKS_STOP'Address));

   ----------------
   -- Count_Task --
   ----------------

   function Count_Task (This : Timer)
                        return Task_Type
   is (Task_Type (This.Periph.TASKS_COUNT'Address));

   ----------------
   -- Clear_Task --
   ----------------

   function Clear_Task (This : Timer)
                        return Task_Type
   is (Task_Type (This.Periph.TASKS_CLEAR'Address));

   ------------------
   -- Capture_Task --
   ------------------

   function Capture_Task (This : Timer;
                          Chan : Timer_Channel)
                          return Task_Type
   is (Task_Type (This.Periph.TASKS_CAPTURE (Integer (Chan))'Address));

   -------------------
   -- Compare_Event --
   -------------------

   function Compare_Event (This : Timer;
                           Chan : Timer_Channel)
                           return Event_Type
   is (Event_Type (This.Periph.EVENTS_COMPARE (Integer (Chan))'Address));

end nRF51.Timers;
