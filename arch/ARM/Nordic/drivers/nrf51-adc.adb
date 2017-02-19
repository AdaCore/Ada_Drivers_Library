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

with NRF51_SVD.ADC; use NRF51_SVD.ADC;
with nRF51.Tasks;   use nRF51.Tasks;

package body nRF51.ADC is

   procedure Set_Resolution (Res : Bits_Resolution);
   procedure Set_Reference (Ref : Reference_Selection);

   --------------------
   -- Set_Resolution --
   --------------------

   procedure Set_Resolution (Res : Bits_Resolution) is
   begin
      ADC_Periph.CONFIG.RES  :=
        (case Res is
            when 8  => CONFIG_RES_Field_8BIT,
            when 9  => CONFIG_RES_Field_9BIT,
            when 10 => CONFIG_RES_Field_10BIT);
   end Set_Resolution;

   -------------------
   -- Set_Reference --
   -------------------

   procedure Set_Reference (Ref : Reference_Selection) is
   begin
      ADC_Periph.CONFIG.EXTREFSEL := None;

      case Ref is
         when Internal_1V2 =>
            ADC_Periph.CONFIG.REFSEL := Vbg;
         when External_AREF0 =>
            ADC_Periph.CONFIG.REFSEL := External;
            ADC_Periph.CONFIG.EXTREFSEL := Analogreference0;
         when External_AREF1 =>
            ADC_Periph.CONFIG.REFSEL := External;
            ADC_Periph.CONFIG.EXTREFSEL := Analogreference1;
         when VDD_Half  =>
            ADC_Periph.CONFIG.REFSEL := Supplyonehalfprescaling;
         when VDD_One_Third  =>
            ADC_Periph.CONFIG.REFSEL := Supplyonethirdprescaling;
      end case;
   end Set_Reference;

   --------------------------
   -- Start_Pin_Conversion --
   --------------------------

   procedure Start_Pin_Conversion
     (Pin   : Analog_Pin;
      Input : Pin_Input_Selection;
      Ref   : Reference_Selection;
      Res   : Bits_Resolution)
   is
   begin
      Set_Resolution (Res);
      Set_Reference (Ref);

      case Input is
         when Pin_Full =>
            ADC_Periph.CONFIG.INPSEL := Analoginputnoprescaling;
         when Pin_Two_Third =>
            ADC_Periph.CONFIG.INPSEL := Analoginputtwothirdsprescaling;
         when Pin_One_Third =>
            ADC_Periph.CONFIG.INPSEL := Analoginputonethirdprescaling;
      end case;

      case Pin is
         when 0 =>
            ADC_Periph.CONFIG.PSEL := Analoginput0;
         when 1 =>
            ADC_Periph.CONFIG.PSEL := Analoginput1;
         when 2 =>
            ADC_Periph.CONFIG.PSEL := Analoginput2;
         when 3 =>
            ADC_Periph.CONFIG.PSEL := Analoginput3;
         when 4 =>
            ADC_Periph.CONFIG.PSEL := Analoginput4;
         when 5 =>
            ADC_Periph.CONFIG.PSEL := Analoginput5;
         when 6 =>
            ADC_Periph.CONFIG.PSEL := Analoginput6;
         when 7 =>
            ADC_Periph.CONFIG.PSEL := Analoginput7;
      end case;

      ADC_Periph.ENABLE.ENABLE := Enabled;
      Trigger (ADC_START);
   end Start_Pin_Conversion;

   --------------------------
   -- Start_VDD_Conversion --
   --------------------------

   procedure Start_VDD_Conversion
     (Input : VDD_Input_Selection;
      Ref   : Reference_Selection;
      Res   : Bits_Resolution)
   is
   begin
      Set_Resolution (Res);
      Set_Reference (Ref);

      ADC_Periph.CONFIG.PSEL := Disabled;

      case Input is
         when VDD_Two_Third =>
            ADC_Periph.CONFIG.INPSEL := Supplytwothirdsprescaling;
         when VDD_One_Third =>
            ADC_Periph.CONFIG.INPSEL := Supplyonethirdprescaling;
      end case;

      ADC_Periph.ENABLE.ENABLE := Enabled;
      Trigger (ADC_START);
   end Start_VDD_Conversion;

   ----------
   -- Busy --
   ----------

   function Busy return Boolean
   is (ADC_Periph.BUSY.BUSY = Busy);

   ---------------------
   -- Wait_For_Result --
   ---------------------

   function Wait_For_Result return UInt10 is
   begin
      while ADC_Periph.BUSY.BUSY = Busy loop
         null;
      end loop;
      ADC_Periph.ENABLE.ENABLE := Enabled;
      return ADC_Periph.RESULT.RESULT;
   end Wait_For_Result;

end nRF51.ADC;
