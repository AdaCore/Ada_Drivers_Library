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

with STM32_SVD.RCC;  use STM32_SVD.RCC;
with STM32_SVD.IWDG; use STM32_SVD.IWDG;

package body STM32.IWDG is

   --  commands to the watchdog hardware

   Reload_Counter : constant Short := 16#AAAA#;
   Enable_Access  : constant Short := 16#5555#;
   Start          : constant Short := 16#CCCC#;

   -------------------------
   -- Initialize_Watchdog --
   -------------------------

   procedure Initialize_Watchdog
     (Prescaler : Prescalers;
      Count     : Countdown_Value)
   is
   begin
      --  Check if we're resuming from a watchdog reset
      if RCC_Periph.CSR.WDGRSTF then
         --  Clear the reset flag
         RCC_Periph.CSR.RMVF := True;
      end if;

      IWDG_Periph.KR.KEY := Enable_Access;
      IWDG_Periph.PR.PR  := Prescalers'Enum_Rep (Prescaler);
      IWDG_Periph.RLR.RL := Count;
   end Initialize_Watchdog;

   --------------------
   -- Start_Watchdog --
   --------------------

   procedure Start_Watchdog is
   begin
      IWDG_Periph.KR.KEY := Reload_Counter;
      IWDG_Periph.KR.KEY := Start;
   end Start_Watchdog;

   --------------------
   -- Reset_Watchdog --
   --------------------

   procedure Reset_Watchdog is
   begin
      while IWDG_Periph.SR.RVU loop
         null;  -- TODO: use a timeout instead of infinitely looping
      end loop;
      IWDG_Periph.KR.KEY := Reload_Counter;
   end Reset_Watchdog;

end STM32.IWDG;
