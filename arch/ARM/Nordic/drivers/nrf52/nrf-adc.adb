------------------------------------------------------------------------------
--                                                                          --
-- Copyright © AdaCore and other contributors, 2017-2020                    --
-- See https://github.com/AdaCore/Ada_Drivers_Library/graphs/contributors   --
-- for more information                                                     --
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

with NRF_SVD.SAADC; use NRF_SVD.SAADC;
with nRF.Tasks;   use nRF.Tasks;
with System.Storage_Elements;

package body nRF.ADC is

   procedure Set_Resolution (Res : Bits_Resolution);
   procedure Set_Reference (Ref : Reference_Selection);
   procedure Wait_For_Result;

   --------------------
   -- Set_Resolution --
   --------------------

   procedure Set_Resolution (Res : Bits_Resolution) is

   begin
      SAADC_Periph.RESOLUTION.VAL :=
        (case Res is
            when Res_8bit  => Val_8BIT,
            when Res_10bit => Val_10BIT,
            when Res_12bit => Val_12BIT);
   end Set_Resolution;

   -------------------
   -- Set_Reference --
   -------------------

   procedure Set_Reference (Ref : Reference_Selection) is
   begin
      case Ref is
         when Internal_0V6 =>
            SAADC_Periph.CH (0).CONFIG.REFSEL := Internal;
         when VDD_One_Forth  =>
            SAADC_Periph.CH (0).CONFIG.REFSEL := Vdd1_4;
      end case;
   end Set_Reference;

   --------------------------
   -- Start_Pin_Conversion --
   --------------------------

   function Do_Pin_Conversion
     (Pin   : Analog_Pin;
      Input : Pin_Input_Selection;
      Ref   : Reference_Selection;
      Res   : Bits_Resolution) return UInt16
   is
      Result : UInt16 with Volatile;
   begin
      Set_Resolution (Res);
      Set_Reference (Ref);

      SAADC_Periph.RESULT.PTR := UInt32 (System.Storage_Elements.To_Integer (Result'Address));
      SAADC_Periph.RESULT.MAXCNT.MAXCNT := 1;

      case Input is
         when Pin_Quadruple =>
            SAADC_Periph.CH (0).CONFIG.GAIN := Gain4;
         when Pin_Double =>
            SAADC_Periph.CH (0).CONFIG.GAIN := Gain2;
         when Pin_Full =>
            SAADC_Periph.CH (0).CONFIG.GAIN := Gain1;
         when Pin_Half =>
            SAADC_Periph.CH (0).CONFIG.GAIN := Gain1_2;
         when Pin_One_Third =>
            SAADC_Periph.CH (0).CONFIG.GAIN := Gain1_3;
         when Pin_One_Forth =>
            SAADC_Periph.CH (0).CONFIG.GAIN := Gain1_4;
         when Pin_One_Fifth =>
            SAADC_Periph.CH (0).CONFIG.GAIN := Gain1_5;
         when Pin_One_Sixth =>
            SAADC_Periph.CH (0).CONFIG.GAIN := Gain1_6;
      end case;

      case Pin is
         when 0 =>
            SAADC_Periph.CH (0).PSELP.PSELP := Analoginput0;
         when 1 =>
            SAADC_Periph.CH (0).PSELP.PSELP := Analoginput1;
         when 2 =>
            SAADC_Periph.CH (0).PSELP.PSELP := Analoginput2;
         when 3 =>
            SAADC_Periph.CH (0).PSELP.PSELP := Analoginput3;
         when 4 =>
            SAADC_Periph.CH (0).PSELP.PSELP := Analoginput4;
         when 5 =>
            SAADC_Periph.CH (0).PSELP.PSELP := Analoginput5;
         when 6 =>
            SAADC_Periph.CH (0).PSELP.PSELP := Analoginput6;
         when 7 =>
            SAADC_Periph.CH (0).PSELP.PSELP := Analoginput7;
      end case;

      SAADC_Periph.ENABLE.ENABLE := Enabled;
      Trigger (ADC_START);
      Wait_For_Result;
      return Result;
   end Do_Pin_Conversion;

   --------------------------
   -- Start_VDD_Conversion --
   --------------------------

   function Do_VDD_Conversion
     (Input : VDD_Input_Selection;
      Ref   : Reference_Selection;
      Res   : Bits_Resolution) return UInt16
   is
      Result : UInt16 with Volatile;
   begin
      Set_Resolution (Res);
      Set_Reference (Ref);

      SAADC_Periph.CH (0).PSELP.PSELP := Vdd;
      SAADC_Periph.RESULT.PTR := UInt32 (System.Storage_Elements.To_Integer (Result'Address));
      SAADC_Periph.RESULT.MAXCNT.MAXCNT := 1;

      case Input is
         when VDD_One_Fifth =>
            SAADC_Periph.CH (0).CONFIG.GAIN := Gain1_5;
         when VDD_One_Sixth =>
            SAADC_Periph.CH (0).CONFIG.GAIN := Gain1_6;
      end case;

      SAADC_Periph.ENABLE.ENABLE := Enabled;
      Trigger (ADC_START);
      Wait_For_Result;
      return Result;
   end Do_VDD_Conversion;

   ----------
   -- Busy --
   ----------

   function Busy return Boolean
   is (SAADC_Periph.STATUS.STATUS = Busy);

   ---------------------
   -- Wait_For_Result --
   ---------------------

   procedure Wait_For_Result is
   begin
      while Busy loop
         null;
      end loop;
      SAADC_Periph.ENABLE.ENABLE := Enabled;
   end Wait_For_Result;

end nRF.ADC;
