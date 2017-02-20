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

with STM32_SVD.RCC;  use STM32_SVD.RCC;
with STM32_SVD.WWDG; use STM32_SVD.WWDG;

package body STM32.WWDG is

   ---------------------------
   -- Enable_Watchdog_Clock --
   ---------------------------

   procedure Enable_Watchdog_Clock is
   begin
      RCC_Periph.APB1ENR.WWDGEN := True;
   end Enable_Watchdog_Clock;

   --------------------
   -- Reset_Watchdog --
   --------------------

   procedure Reset_Watchdog is
   begin
      RCC_Periph.APB1RSTR.WWDGRST := True;
      RCC_Periph.APB1RSTR.WWDGRST := False;
   end Reset_Watchdog;

   ----------------------------
   -- Set_Watchdog_Prescaler --
   ----------------------------

   procedure Set_Watchdog_Prescaler (Value : Prescalers) is
   begin
      WWDG_Periph.CFR.WDGTB.Val := Value'Enum_Rep;
   end Set_Watchdog_Prescaler;

   -------------------------
   -- Set_Watchdog_Window --
   -------------------------

   procedure Set_Watchdog_Window (Window_Start_Count : Downcounter) is
   begin
      WWDG_Periph.CFR.W := Window_Start_Count;
   end Set_Watchdog_Window;

   -----------------------
   -- Activate_Watchdog --
   -----------------------

   procedure Activate_Watchdog (New_Count : Downcounter) is
   begin
      WWDG_Periph.CR.T := New_Count;
      WWDG_Periph.CR.WDGA := True;
   end Activate_Watchdog;

   ------------------------------
   -- Refresh_Watchdog_Counter --
   ------------------------------

   procedure Refresh_Watchdog_Counter (New_Count : Downcounter) is
   begin
      WWDG_Periph.CR.T := New_Count;
   end Refresh_Watchdog_Counter;

   -----------------------------------
   -- Enable_Early_Wakeup_Interrupt --
   -----------------------------------

   procedure Enable_Early_Wakeup_Interrupt is
   begin
      WWDG_Periph.CFR.EWI := True;
   end Enable_Early_Wakeup_Interrupt;

   --------------------------------------
   -- Early_Wakeup_Interrupt_Indicated --
   --------------------------------------

   function Early_Wakeup_Interrupt_Indicated return Boolean is
     (WWDG_Periph.SR.EWIF);

   ----------------------------------
   -- Clear_Early_Wakeup_Interrupt --
   ----------------------------------

   procedure Clear_Early_Wakeup_Interrupt is
   begin
      WWDG_Periph.SR.EWIF := False;
   end Clear_Early_Wakeup_Interrupt;

   --------------------------
   -- WWDG_Reset_Indicated --
   --------------------------

   function WWDG_Reset_Indicated return Boolean is
     (RCC_Periph.CSR.WWDGRSTF);

   ---------------------------
   -- Clear_WWDG_Reset_Flag --
   ---------------------------

   procedure Clear_WWDG_Reset_Flag is
   begin
      RCC_Periph.CSR.RMVF := True;
   end Clear_WWDG_Reset_Flag;

   ------------------
   -- Reset_System --
   ------------------

   procedure Reset_System is
   begin
      WWDG_Periph.CR.T := 0;
      WWDG_Periph.CR.WDGA := True;
   end Reset_System;

end STM32.WWDG;
