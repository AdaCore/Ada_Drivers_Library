------------------------------------------------------------------------------
--                                                                          --
--                        Copyright (C) 2016, AdaCore                       --
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

with STM32_SVD.RTC; use STM32_SVD.RTC;
with STM32_SVD.RCC; use STM32_SVD.RCC;

with STM32.Power_Control;

with HAL.Real_Time_Clock; use HAL.Real_Time_Clock;

package body STM32.RTC is

   procedure Disable_Write_Protection;
   procedure Enable_Write_Protection;

   ------------------------------
   -- Disable_Write_Protection --
   ------------------------------

   procedure Disable_Write_Protection is
   begin
      Power_Control.Disable_Backup_Domain_Protection;
      RTC_Periph.WPR.KEY := 16#CA#;
      RTC_Periph.WPR.KEY := 16#53#;
   end Disable_Write_Protection;

   -----------------------------
   -- Enable_Write_Protection --
   -----------------------------

   procedure Enable_Write_Protection is
   begin
      --  Write any wrong key to enable protection
      RTC_Periph.WPR.KEY := 16#FF#;
      Power_Control.Enable_Backup_Domain_Protection;
   end Enable_Write_Protection;

   ---------
   -- Set --
   ---------

   overriding
   procedure Set (This : in out RTC_Device;
                  Time : HAL.Real_Time_Clock.RTC_Time;
                  Date : HAL.Real_Time_Clock.RTC_Date)
   is
      pragma Unreferenced (This);

      DR : DR_Register;
      TR : TR_Register;
   begin
      Disable_Write_Protection;

      if not RTC_Periph.ISR.INITF then
         --  Enter initialization mode
         RTC_Periph.ISR.INIT := True;

         --  Wait for init mode
         while not RTC_Periph.ISR.INITF loop
            null;
         end loop;
      end if;

      --  RTC prescaler for LSE source clock
      RTC_Periph.PRER.PREDIV_S := 16#FF#;
      RTC_Periph.PRER.PREDIV_A := 16#7F#;

      --  24H format
      TR.PM := False;

      TR.HT := UInt2 (Time.Hour / 10);
      TR.HU := UInt4 (Time.Hour mod 10);

      TR.MNT := UInt3 (Time.Min / 10);
      TR.MNU := UInt4 (Time.Min mod 10);

      TR.ST := UInt3 (Time.Sec / 10);
      TR.SU := UInt4 (Time.Sec mod 10);

      DR.YT := UInt4 (Date.Year / 10);
      DR.YU := UInt4 (Date.Year mod 10);

      DR.WDU := UInt3 (RTC_Day_Of_Week'Enum_Rep (Date.Day_Of_Week));

      DR.MT := (RTC_Month'Enum_Rep (Date.Month) / 10) /= 0;
      DR.MU := UInt4 (RTC_Month'Enum_Rep (Date.Month) mod 10);

      DR.DT := UInt2 (Date.Day / 10);
      DR.DU := UInt4 (Date.Day mod 10);

      --  TR and DR are shadow registers, we have to write them all at once
      RTC_Periph.TR := TR;
      RTC_Periph.DR := DR;

      --  Leave init mode
      RTC_Periph.ISR.INIT := False;

      Enable_Write_Protection;

      while not RTC_Periph.ISR.RSF loop
         null;
      end loop;
   end Set;

   ---------
   -- Get --
   ---------

   overriding
   procedure Get (This : in out RTC_Device;
                  Time : out HAL.Real_Time_Clock.RTC_Time;
                  Date : out HAL.Real_Time_Clock.RTC_Date)
   is
   begin
      loop
         Time := This.Get_Time;
         Date := This.Get_Date;

         --  Check if the time changed when we were reading the date. If it
         --  did, we have to read both values again to ensure coherence.
         exit when This.Get_Time = Time;
      end loop;
   end Get;

   --------------
   -- Get_Time --
   --------------

   overriding
   function Get_Time (This : RTC_Device) return HAL.Real_Time_Clock.RTC_Time
   is
      pragma Unreferenced (This);
      Ret : RTC_Time;
      TR  : constant TR_Register := RTC_Periph.TR;
   begin
      Ret.Hour := RTC_Hour (TR.HT) * 10 + RTC_Hour (RTC_Periph.TR.HU);

      Ret.Min := RTC_Minute (TR.MNT) * 10 + RTC_Minute (TR.MNU);

      Ret.Sec := RTC_Second (TR.ST) * 10 + RTC_Second (TR.SU);

      return Ret;
   end Get_Time;

   --------------
   -- Get_Date --
   --------------

   overriding
   function Get_Date (This : RTC_Device) return HAL.Real_Time_Clock.RTC_Date
   is
      pragma Unreferenced (This);
      Ret : RTC_Date;
      DR  : constant DR_Register := RTC_Periph.DR;
   begin
      Ret.Day_Of_Week := RTC_Day_Of_Week'Enum_Val (DR.WDU);
      Ret.Day := RTC_Day (Integer (DR.DT) * 10 + Integer (DR.DU));
      Ret.Year := RTC_Year (DR.YT) * 10 + RTC_Year (DR.YU);
      Ret.Month := RTC_Month'Enum_Val ((if DR.MT then 10 else 0) + DR.MU);
      return Ret;
   end Get_Date;

   ------------
   -- Enable --
   ------------

   procedure Enable (This : in out RTC_Device) is
      pragma Unreferenced (This);
   begin

      Power_Control.Enable;

      Power_Control.Disable_Backup_Domain_Protection;

      --  Turn on internal low speed oscillator
      RCC_Periph.CSR.LSION := True;

      while not RCC_Periph.CSR.LSIRDY loop
         null;
      end loop;

      --  Select LSI as source clock
      RCC_Periph.BDCR.RTCSEL.Val := 2#10#;
      if RCC_Periph.BDCR.RTCSEL.Val /= 2#10# then
         raise Program_Error with "Cannot select RTC clock";
      end if;

      RCC_Periph.BDCR.RTCEN := True;

      while not RTC_Periph.ISR.RSF loop
         null;
      end loop;

      Power_Control.Enable_Backup_Domain_Protection;
   end Enable;

   -------------
   -- Disable --
   -------------

   procedure Disable (This : in out RTC_Device) is
      pragma Unreferenced (This);
   begin
      Power_Control.Disable_Backup_Domain_Protection;
      RCC_Periph.BDCR.RTCEN := False;
      Power_Control.Enable_Backup_Domain_Protection;
   end Disable;

end STM32.RTC;
