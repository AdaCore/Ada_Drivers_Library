------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2016-2017, AdaCore                     --
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

with STM32_SVD.PWR;       use STM32_SVD.PWR;
with STM32_SVD.RCC;       use STM32_SVD.RCC;
with Cortex_M_SVD.SCB;    use Cortex_M_SVD.SCB;
with System.Machine_Code; use System.Machine_Code;

package body STM32.Power_Control is

   ------------
   -- Enable --
   ------------

   procedure Enable is
   begin
      RCC_Periph.APB1ENR.PWREN := True;
   end Enable;

   --------------------------------------
   -- Disable_Backup_Domain_Protection --
   --------------------------------------

   procedure Disable_Backup_Domain_Protection is
   begin
      PWR_Periph.CR.DBP := True;
   end Disable_Backup_Domain_Protection;

   -------------------------------------
   -- Enable_Backup_Domain_Protection --
   -------------------------------------

   procedure Enable_Backup_Domain_Protection is
   begin
      PWR_Periph.CR.DBP := False;
   end Enable_Backup_Domain_Protection;

   -----------------
   -- Wakeup_Flag --
   -----------------

   function Wakeup_Flag return Boolean
   is (PWR_Periph.CSR.WUF);

   -----------------------
   -- Clear_Wakeup_Flag --
   -----------------------

   procedure Clear_Wakeup_Flag is
   begin
      PWR_Periph.CR.CWUF := True;
   end Clear_Wakeup_Flag;

   ------------------
   -- Standby_Flag --
   ------------------

   function Standby_Flag return Boolean
   is (PWR_Periph.CSR.SBF);

   ------------------------
   -- Clear_Standby_Flag --
   ------------------------

   procedure Clear_Standby_Flag is
   begin
      PWR_Periph.CR.CSBF := True;
   end Clear_Standby_Flag;

   ------------------------------
   -- Set_Power_Down_Deepsleep --
   ------------------------------

   procedure Set_Power_Down_Deepsleep (Enabled : Boolean := True) is
   begin
      PWR_Periph.CR.PDDS := Enabled;
   end Set_Power_Down_Deepsleep;

   -----------------------------
   -- Set_Low_Power_Deepsleep --
   -----------------------------

   procedure Set_Low_Power_Deepsleep (Enabled : Boolean := True) is
   begin
      PWR_Periph.CR.LPDS := Enabled;
   end Set_Low_Power_Deepsleep;

   -----------------------
   -- Enable_Wakeup_Pin --
   -----------------------

   procedure Enable_Wakeup_Pin (Enabled : Boolean := True) is
   begin
      PWR_Periph.CSR.EWUP := Enabled;
   end Enable_Wakeup_Pin;

   ------------------------
   -- Enter_Standby_Mode --
   ------------------------

   procedure Enter_Standby_Mode is
   begin
      Clear_Wakeup_Flag;
      Clear_Standby_Flag;

      Set_Power_Down_Deepsleep;

      SCB_Periph.SCR.SLEEPDEEP := True;

      loop
         Asm ("wfi", Volatile => True);
      end loop;
   end Enter_Standby_Mode;

end STM32.Power_Control;
