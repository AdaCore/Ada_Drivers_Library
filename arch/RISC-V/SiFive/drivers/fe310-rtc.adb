------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2018, AdaCore and other contributors            --
--                                                                          --
--      See github.com/AdaCore/Ada_Drivers_Library/graphs/contributors      --
--                           for more information                           --
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

with FE310_SVD.RTC; use FE310_SVD.RTC;

package body FE310.RTC is

   -----------
   -- Count --
   -----------

   function Count return Count_Value is
      Low_Reg : UInt32;
      High_Reg : HI_Register;
   begin
      High_Reg := RTC_Periph.HI;
      Low_Reg := RTC_Periph.LO;

      --  Handle the case where the timer registers were read during the
      --  incrementation of the high part
      if RTC_Periph.HI /= High_Reg then
         High_Reg.CNT := High_Reg.CNT + 1;
         Low_Reg := 0;
      end if;

      return Count_Value (High_Reg.CNT) * 2**32 + Count_Value (Low_Reg);
   end Count;

   ---------------
   -- Set_Count --
   ---------------

   procedure Set_Count (Value : Count_Value) is
   begin
      RTC_Periph.CONFIG.ENALWAYS := False;
      RTC_Periph.HI.CNT := UInt16 (Value / 2**32);
      RTC_Periph.LO := UInt32 (Value rem 2**32);
      RTC_Periph.CONFIG.ENALWAYS := True;
   end Set_Count;

   ----------------------
   -- Enable_Continous --
   ----------------------

   procedure Enable is
   begin
      RTC_Periph.CONFIG.ENALWAYS := True;
   end Enable;

   -------------
   -- Disable --
   -------------

   procedure Disable is
   begin
      RTC_Periph.CONFIG.ENALWAYS := False;
   end Disable;

   --------------------
   -- Scaled_Counter --
   --------------------

   function Scaled_Counter return Scaled_Value
   is (RTC_Periph.SCALE_COUNT);

   ---------------
   -- Configure --
   ---------------

   procedure Set_Scale (Scale : FE310_SVD.RTC.CONFIG_SCALE_Field) is
   begin
      RTC_Periph.CONFIG.SCALE := Scale;
   end Set_Scale;

   -----------------
   -- Set_Compare --
   -----------------

   procedure Set_Compare (Value : Compare_Value) is
   begin
      RTC_Periph.COMPARE := Value;
   end Set_Compare;

   -------------
   -- Compare --
   -------------

   function Compare return Compare_Value
   is (RTC_Periph.COMPARE);

   -----------------------
   -- Interrupt_Pending --
   -----------------------

   function Interrupt_Pending return Boolean
   is (RTC_Periph.CONFIG.CMP_IP);


end FE310.RTC;
