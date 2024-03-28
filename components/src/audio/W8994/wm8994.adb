------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2015-2024, AdaCore                     --
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
with WM8994.IO; use WM8994.IO;

package body WM8994 is

   WM8994_CHIPID_ADDR : constant := 16#00#;

   Output_Enabled     : Boolean := False;
   Input_Enabled      : Boolean := False;
   pragma Unreferenced (Input_Enabled);

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This      : in out WM8994_Device;
      Input     : Input_Device;
      Output    : Output_Device;
      Volume    : Volume_Level;
      Frequency : Audio_Frequency)
   is
      Power_Mgnt_Reg_1 : UInt16 := 0;
   begin
      --  WM8994 Errata work-arounds. See
      --  https://github.com/STMicroelectronics/stm32-wm8994/blob/main/wm8994.c
      --  These registers are not documented, and the effects of these writes
      --  are not documented anywhere either. Nobody seems to have written it
      --  down anywhere.
      I2C_Write (This, 16#102#, 16#0003#);
      I2C_Write (This, 16#817#, 16#0000#);
      I2C_Write (This, 16#102#, 16#0000#);

      --  Enable VMID soft restart, Start-up Bias current enabled
      I2C_Write (This, WM8994_AntiPop2, 16#006C#);

      --  Enable BIAS generator, Enable VMID
      I2C_Write (This, WM8994_PWR_Management_1, 16#0003#);

      This.Time.Delay_Milliseconds (50);

      Output_Enabled := Output /= No_Output;
      Input_Enabled  := Input /= No_Input;

      This.Set_Output_Mode (Output);

      case Input is
         when No_Input =>
            null;

         when Microphone =>
            --  Enable AIF1ADC2 (Left), Enable AIF1ADC2 (Right)
            --  Enable DMICDAT2 (Left), Enable DMICDAT2 (Right)
            --  Enable Left ADC, Enable Right ADC
            I2C_Write (This, WM8994_PWR_Management_4, 16#0C30#);
            --  Enable AIF1 DRC2 Signal Detect & DRC in AIF1ADC2 Left/Right
            --  Timeslot 1
            I2C_Write (This, WM8994_AIF1_DRC2, 16#00DB#);
            --  Disable IN1L, IN1R, IN2L, IN2R, Enable Thermal sensor &
            --  shutdown
            I2C_Write (This, WM8994_PWR_Management_4, 16#6000#);
            --  Enable the DMIC2(Left) to AIF1 Timeslot 1 (Left) mixer path
            I2C_Write (This, WM8994_AIF1_ADC2_LMR, 16#0002#);
            --  Enable the DMIC2(Right) to AIF1 Timeslot 1 (Right) mixer path
            I2C_Write (This, WM8994_AIF1_ADC2_RMR, 16#0002#);
            --  GPIO1 pin configuration GP1_DIR = output, GP1_FN = AIF1 DRC2
            --  signal detect
            I2C_Write (This, WM8994_GPIO1, 16#000E#);

         when Input_Line =>
            --  Enable AIF1ADC1 (Left), Enable AIF1ADC1 (Right)
            --  Enable Left ADC, Enable Right ADC
            I2C_Write (This, WM8994_PWR_Management_4, 16#0303#);
            --  Enable AIF1 DRC1 Signal Detect & DRC in AIF1ADC1 Left/Right
            --  Timeslot 0
            I2C_Write (This, WM8994_AIF1_DRC1, 16#00DB#);
            --  Enable IN1L and IN1R, Disable IN2L and IN2R, Enable Thermal
            --  sensor & shutdown
            I2C_Write (This, WM8994_PWR_Management_4, 16#6350#);
            --  Enable the ADCL(Left) to AIF1 Timeslot 0 (Left) mixer path
            I2C_Write (This, WM8994_AIF1_ADC1_LMR, 16#0002#);
            --  Enable the ADCR(Right) to AIF1 Timeslot 0 (Right) mixer path
            I2C_Write (This, WM8994_AIF1_ADC1_RMR, 16#0002#);
            --  GPIO1 pin configuration GP1_DIR = output, GP1_FN = AIF1 DRC1
            --  signal detect
            I2C_Write (This, WM8994_GPIO1, 16#000D#);
      end case;

      This.Set_Frequency (Frequency);

      --  AIF1 Word Length = 16-bits, AIF1 Format = I2S (Default Register
      --  Value)
      I2C_Write (This, WM8994_AIF1_Control1, 16#4010#);
      --  slave mode
      I2C_Write (This, WM8994_AIF1_Master_Slave, 16#0000#);

      --  Enable the DSP processing clock for AIF1, Enable the core clock
      I2C_Write (This, WM8994_Clocking1, 16#000A#);

      --  Enable AIF1 Clock, AIF1 Clock Source = MCLK1 pin
      I2C_Write (This, WM8994_AIF1_Clocking1, 16#0001#);

      if Output /= No_Output then
         --  Analog Output Configuration

         --  Enable SPKRVOL PGA, Enable SPKMIXR, Enable SPKLVOL PGA, Enable
         --  SPKMIXL
         I2C_Write (This, 16#03#, 16#0300#);

         --  Left Speaker Mixer Volume = 0dB
         I2C_Write (This, WM8994_SPKMIXL_ATT, 16#0000#);

         --  Speaker output mode = Class D, Right Speaker Mixer Volume = 0dB
         --  ((16#23#, 16#0100#) = class AB)
         I2C_Write (This, WM8994_SPKMIXR_ATT, 16#0000#);

         --  Unmute DAC2 (Left) to Left Speaker Mixer (SPKMIXL) path,
         --  Unmute DAC2 (Right) to Right Speaker Mixer (SPKMIXR) path
         I2C_Write (This, WM8994_Speaker_Mixer, 16#0300#);

         --  Enable bias generator, Enable VMID, Enable SPKOUTL, Enable SPKOUTR
         I2C_Write (This, WM8994_PWR_Management_1, 16#3003#);

         --  Headphone/Speaker Enable

         --  Enable Class W, Class W Envelope Tracking = AIF1 Timeslot 0
         I2C_Write (This, WM8994_CLASS_W, 16#0001#);

         --  Enable bias generator, Enable VMID, Enable HPOUT1 (Left) and
         --  Enable HPOUT1 (Right) input stages idem for Speaker
         Power_Mgnt_Reg_1 := Power_Mgnt_Reg_1 or 16#0303# or 16#3003#;
         I2C_Write (This, WM8994_PWR_Management_1, Power_Mgnt_Reg_1);

         --  Enable HPOUT1 (Left) and HPOUT1 (Right) intermediate stages
         I2C_Write (This, WM8994_Analog_HP, 16#0022#);

         --  Enable Charge Pump
         I2C_Write (This, WM8994_Charge_Pump1, 16#9F25#);

         --  Add Delay
         This.Time.Delay_Milliseconds (15);

         --  Select DAC1 (Left) to Left Headphone Output PGA (HPOUT1LVOL) path
         I2C_Write (This, WM8994_Output_Mixer_1, 16#0001#);

         --  Select DAC1 (Right) to Right Headphone Output PGA (HPOUT1RVOL)
         --  path.
         I2C_Write (This, WM8994_Output_Mixer_2, 16#0001#);

         --  Enable Left Output Mixer (MIXOUTL), Enable Right Output Mixer
         --  (MIXOUTR) idem for SPKOUTL and SPKOUTR.
         I2C_Write (This, WM8994_PWR_Management_3, 16#0030# or 16#0300#);

         --  Enable DC Servo and trigger start-up mode on left and right
         --  channels.
         I2C_Write (This, WM8994_DC_Servo1, 16#0033#);

         --  Add Delay
         This.Time.Delay_Milliseconds (250);

         --  Enable HPOUT1 (Left) and HPOUT1 (Right) intermediate and output
         --  stages. Remove clamps.
         I2C_Write (This, WM8994_Analog_HP, 16#00EE#);

         --  Unmutes

         --  Unmute DAC 1 (Left)
         I2C_Write (This, WM8994_DAC1_Left_Vol, 16#00C0#);

         --  Unmute DAC 1 (Right)
         I2C_Write (This, WM8994_DAC1_Right_Vol, 16#00C0#);

         --  Unmute the AIF1 Timeslot 0 DAC path
         I2C_Write (This, WM8994_AIF1_DAC1_Filter1, 16#0000#);

         --  Unmute DAC 2 (Left)
         I2C_Write (This, WM8994_DAC2_Left_Vol, 16#00C0#);

         --  Unmute DAC 2 (Right)
         I2C_Write (This, WM8994_DAC2_Right_Vol, 16#00C0#);

         --  Unmute the AIF1 Timeslot 1 DAC2 path
         I2C_Write (This, WM8994_AIF1_DAC2_Filter1, 16#0000#);

         --  Volume Control
         This.Set_Volume (Volume);
      end if;

      if Input /= No_Input then
         if Input = Microphone then
            --  Enable Microphone bias 1 generator, Enable VMID
            Power_Mgnt_Reg_1 := Power_Mgnt_Reg_1 or 16#0013#;
            I2C_Write (This, WM8994_PWR_Management_1, Power_Mgnt_Reg_1);

            --  ADC oversample enable
            I2C_Write (This, WM8994_Oversampling, 16#0002#);

            --  AIF ADC2 HPF enable, HPF cut = voice mode 1 fc=127Hz at fs=8kHz
            I2C_Write (This, WM8994_AIF1_ADC2_Filters, 16#3800#);

         elsif Input = Input_Line then
            --  Enable normal bias generator, Enable VMID
            Power_Mgnt_Reg_1 := Power_Mgnt_Reg_1 or 16#0003#;
            I2C_Write (This, WM8994_PWR_Management_1, Power_Mgnt_Reg_1);

            --  Disable mute on IN1L, IN1L Volume = +0dB
            I2C_Write (This, WM8994_Left_Line_In12_Vol, 16#000B#);

            --  Disable mute on IN1R, IN1R Volume = +0dB
            I2C_Write (This, WM8994_Right_Line_In12_Vol, 16#000B#);

            --  Disable mute on IN1L_TO_MIXINL, Gain = +0dB
            I2C_Write (This, WM8994_Input_Mixer_3, 16#0025#);

            --  Disable mute on IN1R_TO_MIXINL, Gain = +0dB
            I2C_Write (This, WM8994_Input_Mixer_4, 16#0025#);

            --  IN1LN_TO_IN1L, IN1LP_TO_VMID, IN1RN_TO_IN1R, IN1RP_TO_VMID
            I2C_Write (This, WM8994_Input_Mixer_2, 16#0011#);

            --  AIF ADC1 HPF enable, HPF cut = hifi mode fc=4Hz at fs=48kHz
            I2C_Write (This, WM8994_AIF1_ADC1_Filters, 16#1800#);
         end if;

         --  Volume Control
         This.Set_Volume (Volume);
      end if;
   end Initialize;

   -------------
   -- Chip_ID --
   -------------

   function Chip_ID (This : in out WM8994_Device) return UInt16 is
   begin
      return I2C_Read (This, WM8994_CHIPID_ADDR);
   end Chip_ID;

   ----------
   -- Play --
   ----------

   procedure Play (This : in out WM8994_Device) is
   begin
      This.Set_Mute (Mute_Off);
   end Play;

   -----------
   -- Pause --
   -----------

   procedure Pause (This : in out WM8994_Device) is
   begin
      --  Pause the audio playing
      This.Set_Mute (Mute_On);

      --  CODEC in powersave mode
      I2C_Write (This, WM8994_PWR_Management_2, 16#01#);
   end Pause;

   ------------
   -- Resume --
   ------------

   procedure Resume (This : in out WM8994_Device) is
   begin
      This.Set_Mute (Mute_Off);
   end Resume;

   ----------
   -- Stop --
   ----------

   procedure Stop (This : in out WM8994_Device; Cmd : Stop_Mode) is
   begin
      if Output_Enabled then
         --  Mute the output first
         This.Set_Mute (Mute_On);

         if Cmd = Stop_Power_Down_Sw then
            return;
         end if;

         Output_Enabled := False;

         --  Mute the AIF1 Timeslot 0 DAC1 path
         I2C_Write (This, WM8994_AIF1_DAC1_Filter1, 16#0200#);

         --  Mute the AIF1 Timeslot 1 DAC2 path
         I2C_Write (This, WM8994_AIF1_DAC2_Filter1, 16#0200#);

         --  Disable DAC1L_TO_HPOUT1L
         I2C_Write (This, WM8994_Output_Mixer_1, 16#0000#);

         --  Disable DAC1R_TO_HPOUT1R
         I2C_Write (This, WM8994_Output_Mixer_2, 16#0000#);

         --  Disable DAC1 and DAC2
         I2C_Write (This, WM8994_PWR_Management_5, 16#0000#);

         --  Reset Codec
         I2C_Write (This, WM8994_SW_Reset, 16#0000#);
      end if;
   end Stop;

   ----------------
   -- Set_Volume --
   ----------------

   procedure Set_Volume
     (This   : in out WM8994_Device;
      Volume : Volume_Level)
   is
   begin
      if Volume = 0 then
         --  Mute the codec
         This.Set_Mute (Mute_On);
      else
         This.Set_Mute (Mute_Off);

         --  Left Headphone Volume
         I2C_Write (This, WM8994_Left_Output_Vol, Volume or 16#140#);

         --  Right Headphone volume
         I2C_Write (This, WM8994_Right_Output_Vol, Volume or 16#140#);

         --  Left Speaker volume
         I2C_Write (This, WM8994_SPK_Left_Vol, Volume or 16#140#);

         --  Right Speaker volume
         I2C_Write (This, WM8994_SPK_Right_Vol, Volume or 16#140#);
      end if;
   end Set_Volume;

   --------------
   -- Set_Mute --
   --------------

   procedure Set_Mute (This : in out WM8994_Device; Cmd : Mute) is
   begin
      if Output_Enabled then
         case Cmd is
            when Mute_On =>
               --  Soft Mute the AIF1 Timeslot 0 DAC1 path L&R
               I2C_Write (This, WM8994_AIF1_DAC1_Filter1, 16#0200#);
               --  Soft Mute the AIF1 Timeslot 1 DAC2 path L&R
               I2C_Write (This, WM8994_AIF1_DAC2_Filter1, 16#0200#);
            when Mute_Off =>
               --  Unmute the AIF1 Timeslot 0 DAC1 path L&R
               I2C_Write (This, WM8994_AIF1_DAC1_Filter1, 16#0000#);
               --  Unmute the AIF1 Timeslot 1 DAC2 path L&R
               I2C_Write (This, WM8994_AIF1_DAC2_Filter1, 16#0000#);
         end case;
      end if;
   end Set_Mute;

   ---------------------
   -- Set_Output_Mode --
   ---------------------

   procedure Set_Output_Mode
     (This   : in out WM8994_Device;
      Device : Output_Device)
   is
   begin
      case Device is
         when No_Output =>
            --  Disable DAC1 (left), DAC1 (Right)
            I2C_Write (This, WM8994_PWR_Management_5, 16#0000#);
            --  Mute the AIF1 Timeslot 0 DAC1 path
            I2C_Write (This, WM8994_AIF1_DAC1_Filter1, 16#0200#);
            --  Mute the AIF1 Timeslot 1 DAC2 path
            I2C_Write (This, WM8994_AIF1_DAC2_Filter1, 16#0200#);

         when Speaker =>
            --  Enable DAC1 (left), DAC1 (Right)
            I2C_Write (This, WM8994_PWR_Management_5, 16#0C0C#);
            --  Enable the AIF1 Timeslot 0 (Left) to DAC1 (left) mixer path
            I2C_Write (This, WM8994_AIF1_DAC1_LMR, 16#0000#);
            --  Enable the AIF1 Timeslot 0 (Right) to DAC 1 (Right) mixer path
            I2C_Write (This, WM8994_AIF1_DAC1_RMR, 16#0000#);
            --  Disable the AIF1 Timeslot 1 (Left) to DAC 2 (Left) mixer path
            I2C_Write (This, WM8994_AIF1_DAC2_LMR, 16#0002#);
            --  Disable the AIF1 Timeslot 1 (Right) to DAC 2 (Right) mixer path
            I2C_Write (This, WM8994_AIF1_DAC2_RMR, 16#0002#);

         when Headphone | Auto =>
            --  Disable DAC1 (left), DAC1 (Right)
            --  Enable DAC2 (left), DAC2 (Right)
            I2C_Write (This, WM8994_PWR_Management_5, 16#0303#);
            --  Enable the AIF1 Timeslot 0 (Left) to DAC1 (left) mixer path
            I2C_Write (This, WM8994_AIF1_DAC1_LMR, 16#0001#);
            --  Enable the AIF1 Timeslot 0 (Right) to DAC 1 (Right) mixer path
            I2C_Write (This, WM8994_AIF1_DAC1_RMR, 16#0001#);
            --  Disable the AIF1 Timeslot 1 (Left) to DAC 2 (Left) mixer path
            I2C_Write (This, WM8994_AIF1_DAC2_LMR, 16#0000#);
            --  Disable the AIF1 Timeslot 1 (Right) to DAC 2 (Right) mixer path
            I2C_Write (This, WM8994_AIF1_DAC2_RMR, 16#0000#);

         when Both =>
            --  Enable DAC1 (left), DAC1 (Right)
            --  Enable DAC2 (left), DAC2 (Right)
            I2C_Write (This, WM8994_PWR_Management_5, 16#0303# or 16#0C0C#);
            --  Enable the AIF1 Timeslot 0 (Left) to DAC1 (left) mixer path
            I2C_Write (This, WM8994_AIF1_DAC1_LMR, 16#0001#);
            --  Enable the AIF1 Timeslot 0 (Right) to DAC 1 (Right) mixer path
            I2C_Write (This, WM8994_AIF1_DAC1_RMR, 16#0001#);
            --  Enable the AIF1 Timeslot 1 (Left) to DAC 2 (Left) mixer path
            I2C_Write (This, WM8994_AIF1_DAC2_LMR, 16#0002#);
            --  Enable the AIF1 Timeslot 1 (Right) to DAC 2 (Right) mixer path
            I2C_Write (This, WM8994_AIF1_DAC2_RMR, 16#0002#);
      end case;
   end Set_Output_Mode;

   -------------------
   -- Set_Frequency --
   -------------------

   procedure Set_Frequency
     (This  : in out WM8994_Device;
      Freq : Audio_Frequency)
   is
   begin
      --  In the following, the values written set both the AIF1_SR [3:0] bits
      --  and the AIF1CLK_RATE [3:0] bits (the latter to set the ratio). The
      --  ratio is always 256, which is indicated by the bit pattern 2#0011#,
      --  so the lower digits in the values is always 3 in hex.
      --
      --  See the table labled "Register 0210h AIF1 Rate" pages 285 and 286 of
      --  WM8994_Rev4.6 from Cirrus Logic
      case Freq is
         when Audio_Freq_8kHz =>
            --  AIF1 Sample Rate = 8 (kHz), ratio=256
            I2C_Write (This, WM8994_AIF1_Rate, 16#0003#);

         when Audio_Freq_11kHz =>
            --  AIF1 Sample Rate = 11.025 (kHz), ratio=256
            I2C_Write (This, WM8994_AIF1_Rate, 16#0013#);

         when Audio_Freq_12kHz =>
            --  AIF1 Sample Rate = 12 (kHz), ratio=256
            I2C_Write (This, WM8994_AIF1_Rate, 16#0023#);

         when Audio_Freq_16kHz =>
            --  AIF1 Sample Rate = 16 (kHz), ratio=256
            I2C_Write (This, WM8994_AIF1_Rate, 16#0033#);

         when Audio_Freq_22kHz =>
            --  AIF1 Sample Rate = 22.050 (kHz), ratio=256
            I2C_Write (This, WM8994_AIF1_Rate, 16#0043#);

         when Audio_Freq_24kHz =>
            --  AIF1 Sample Rate = 24 (kHz), ratio=256
            I2C_Write (This, WM8994_AIF1_Rate, 16#0053#);

         when Audio_Freq_32kHz =>
            --  AIF1 Sample Rate = 32 (kHz), ratio=256
            I2C_Write (This, WM8994_AIF1_Rate, 16#0063#);

         when Audio_Freq_44kHz =>
            --  AIF1 Sample Rate = 44.1 (kHz), ratio=256
            I2C_Write (This, WM8994_AIF1_Rate, 16#0073#);

         when Audio_Freq_48kHz =>
            --  AIF1 Sample Rate = 48 (kHz), ratio=256
            I2C_Write (This, WM8994_AIF1_Rate, 16#0083#);

         when Audio_Freq_88kHz =>
            --  AIF1 Sample Rate = 88.2 (kHz), ratio=256
            I2C_Write (This, WM8994_AIF1_Rate, 16#0093#);

         when Audio_Freq_96kHz =>
            --  AIF1 Sample Rate = 96 (kHz), ratio=256
            I2C_Write (This, WM8994_AIF1_Rate, 16#00A3#);
      end case;
   end Set_Frequency;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : in out WM8994_Device) is
   begin
      I2C_Write (This, WM8994_SW_Reset, 16#0000#);
      Output_Enabled := False;
      Input_Enabled  := False;
   end Reset;

end WM8994;
