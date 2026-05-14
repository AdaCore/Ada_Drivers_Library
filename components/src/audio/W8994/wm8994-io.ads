------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2026, AdaCore                          --
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

package WM8994.IO is

   type Register_Address is new UInt16;

   procedure I2C_Write
     (This     : in out Audio_CODEC;
      Register : Register_Address;
      Value    : UInt16);

   function I2C_Read
     (This     : in out Audio_CODEC;
      Register : Register_Address)
      return UInt16;

   WM8994_SW_Reset            : constant Register_Address := 16#0000#;
   WM8994_PWR_Management_1    : constant Register_Address := 16#0001#;
   WM8994_PWR_Management_2    : constant Register_Address := 16#0002#;
   WM8994_PWR_Management_3    : constant Register_Address := 16#0003#;
   WM8994_PWR_Management_4    : constant Register_Address := 16#0004#;
   WM8994_PWR_Management_5    : constant Register_Address := 16#0005#;
   WM8994_PWR_Management_6    : constant Register_Address := 16#0006#;
   WM8994_Input_Mixer_1       : constant Register_Address := 16#0015#;
   WM8994_Left_Line_In12_Vol  : constant Register_Address := 16#0018#;
   WM8994_Left_Line_In34_Vol  : constant Register_Address := 16#0019#;
   WM8994_Right_Line_In12_Vol : constant Register_Address := 16#001A#;
   WM8994_Right_Line_In34_Vol : constant Register_Address := 16#001B#;
   WM8994_Left_Output_Vol     : constant Register_Address := 16#001C#;
   WM8994_Right_Output_Vol    : constant Register_Address := 16#001D#;
   WM8994_Line_Output_Vol     : constant Register_Address := 16#001E#;
   WM8994_Output2_Vol         : constant Register_Address := 16#001F#;
   WM8994_Left_OPGA_Vol       : constant Register_Address := 16#0020#;
   WM8994_Right_OPGA_Vol      : constant Register_Address := 16#0021#;
   WM8994_SPKMIXL_ATT         : constant Register_Address := 16#0022#;
   WM8994_SPKMIXR_ATT         : constant Register_Address := 16#0023#;
   WM8994_Output_Mixer        : constant Register_Address := 16#0024#;
   WM8994_CLASS_D             : constant Register_Address := 16#0025#;
   WM8994_SPK_Left_Vol        : constant Register_Address := 16#0026#;
   WM8994_SPK_Right_Vol       : constant Register_Address := 16#0027#;
   WM8994_Input_Mixer_2       : constant Register_Address := 16#0028#;
   WM8994_Input_Mixer_3       : constant Register_Address := 16#0029#;
   WM8994_Input_Mixer_4       : constant Register_Address := 16#002A#;
   WM8994_Input_Mixer_5       : constant Register_Address := 16#002B#;
   WM8994_Input_Mixer_6       : constant Register_Address := 16#002C#;
   WM8994_Output_Mixer_1      : constant Register_Address := 16#002D#;
   WM8994_Output_Mixer_2      : constant Register_Address := 16#002E#;
   WM8994_Output_Mixer_3      : constant Register_Address := 16#002F#;
   WM8994_Output_Mixer_4      : constant Register_Address := 16#0030#;
   WM8994_Output_Mixer_5      : constant Register_Address := 16#0031#;
   WM8994_Output_Mixer_6      : constant Register_Address := 16#0032#;
   WM8994_Output2_Mixer       : constant Register_Address := 16#0033#;
   WM8994_Line_Mixer_1        : constant Register_Address := 16#0034#;
   WM8994_Line_Mixer_2        : constant Register_Address := 16#0035#;
   WM8994_Speaker_Mixer       : constant Register_Address := 16#0036#;
   WM8994_Add_Control         : constant Register_Address := 16#0037#;
   WM8994_AntiPop1            : constant Register_Address := 16#0038#;
   WM8994_AntiPop2            : constant Register_Address := 16#0039#;
   WM8994_MicBias             : constant Register_Address := 16#003A#;
   WM8994_LDO1                : constant Register_Address := 16#003B#;
   WM8994_LDO2                : constant Register_Address := 16#003C#;
   WM8994_Charge_Pump1        : constant Register_Address := 16#004C#;
   WM8994_Charge_Pump2        : constant Register_Address := 16#004D#;
   WM8994_CLASS_W             : constant Register_Address := 16#0051#;
   WM8994_DC_Servo1           : constant Register_Address := 16#0054#;
   WM8994_DC_Servo2           : constant Register_Address := 16#0055#;
   WM8994_DC_Servo_Readback   : constant Register_Address := 16#0058#;
   WM8994_DC_Servo_Writeval   : constant Register_Address := 16#0059#;
   WM8994_Analog_HP           : constant Register_Address := 16#0060#;
   WM8994_Chip_Revision       : constant Register_Address := 16#0100#;
   WM8994_Control_Interface   : constant Register_Address := 16#0101#;
   WM8994_WRITE_SEQ_CTRL1     : constant Register_Address := 16#0110#;
   WM8994_WRITE_SEQ_CTRL2     : constant Register_Address := 16#0111#;
   WM8994_AIF1_Clocking1      : constant Register_Address := 16#0200#;
   WM8994_AIF1_Clocking2      : constant Register_Address := 16#0201#;
   WM8994_AIF2_Clocking1      : constant Register_Address := 16#0204#;
   WM8994_AIF2_Clocking2      : constant Register_Address := 16#0205#;
   WM8994_Clocking1           : constant Register_Address := 16#0208#;
   WM8994_Clocking2           : constant Register_Address := 16#0209#;
   WM8994_AIF1_Rate           : constant Register_Address := 16#0210#;
   WM8994_AIF2_Rate           : constant Register_Address := 16#0211#;
   WM8994_Rate_Status         : constant Register_Address := 16#0212#;
   WM8994_FLL1_Control1       : constant Register_Address := 16#0220#;
   WM8994_FLL1_Control2       : constant Register_Address := 16#0221#;
   WM8994_FLL1_Control3       : constant Register_Address := 16#0222#;
   WM8994_FLL1_Control4       : constant Register_Address := 16#0223#;
   WM8994_FLL1_Control5       : constant Register_Address := 16#0224#;
   WM8994_FLL2_Control1       : constant Register_Address := 16#0240#;
   WM8994_FLL2_Control2       : constant Register_Address := 16#0241#;
   WM8994_FLL2_Control3       : constant Register_Address := 16#0242#;
   WM8994_FLL2_Control4       : constant Register_Address := 16#0243#;
   WM8994_FLL2_Control5       : constant Register_Address := 16#0244#;
   WM8994_AIF1_Control1       : constant Register_Address := 16#0300#;
   WM8994_AIF1_Control2       : constant Register_Address := 16#0301#;
   WM8994_AIF1_Master_Slave   : constant Register_Address := 16#0302#;
   WM8994_AIF1_BCLK           : constant Register_Address := 16#0303#;
   WM8994_AIF1_ADC_LRCLK      : constant Register_Address := 16#0304#;
   WM8994_AIF1_DAC_LRCLK      : constant Register_Address := 16#0305#;
   WM8994_AIF1_DAC_DELTA      : constant Register_Address := 16#0306#;
   WM8994_AIF1_ADC_DELTA      : constant Register_Address := 16#0307#;
   WM8994_AIF2_Control1       : constant Register_Address := 16#0310#;
   WM8994_AIF2_Control2       : constant Register_Address := 16#0311#;
   WM8994_AIF2_Master_Slave   : constant Register_Address := 16#0312#;
   WM8994_AIF2_BCLK           : constant Register_Address := 16#0313#;
   WM8994_AIF2_ADC_LRCLK      : constant Register_Address := 16#0314#;
   WM8994_AIF2_DAC_LRCLK      : constant Register_Address := 16#0315#;
   WM8994_AIF2_DAC_DELTA      : constant Register_Address := 16#0316#;
   WM8994_AIF2_ADC_DELTA      : constant Register_Address := 16#0317#;
   WM8994_AIF1_ADC1_Left_Vol  : constant Register_Address := 16#0400#;
   WM8994_AIF1_ADC1_Right_Vol : constant Register_Address := 16#0401#;
   WM8994_AIF1_DAC1_Left_Vol  : constant Register_Address := 16#0402#;
   WM8994_AIF1_DAC1_Right_Vol : constant Register_Address := 16#0403#;
   WM8994_AIF1_ADC2_Left_Vol  : constant Register_Address := 16#0404#;
   WM8994_AIF1_ADC2_Right_Vol : constant Register_Address := 16#0405#;
   WM8994_AIF1_DAC2_Left_Vol  : constant Register_Address := 16#0406#;
   WM8994_AIF1_DAC2_Right_Vol : constant Register_Address := 16#0407#;
   WM8994_AIF1_ADC1_Filters   : constant Register_Address := 16#0410#;
   WM8994_AIF1_ADC2_Filters   : constant Register_Address := 16#0411#;
   WM8994_AIF1_DAC1_Filter1   : constant Register_Address := 16#0420#;
   WM8994_AIF1_DAC1_Filter2   : constant Register_Address := 16#0421#;
   WM8994_AIF1_DAC2_Filter1   : constant Register_Address := 16#0422#;
   WM8994_AIF1_DAC2_Filter2   : constant Register_Address := 16#0423#;
   WM8994_AIF1_DRC1           : constant Register_Address := 16#0440#;
   WM8994_AIF1_DRC1_1         : constant Register_Address := 16#0441#;
   WM8994_AIF1_DRC1_2         : constant Register_Address := 16#0442#;
   WM8994_AIF1_DRC1_3         : constant Register_Address := 16#0443#;
   WM8994_AIF1_DRC1_4         : constant Register_Address := 16#0444#;
   WM8994_AIF1_DRC2           : constant Register_Address := 16#0450#;
   WM8994_AIF1_DRC2_1         : constant Register_Address := 16#0451#;
   WM8994_AIF1_DRC2_2         : constant Register_Address := 16#0452#;
   WM8994_AIF1_DRC2_3         : constant Register_Address := 16#0453#;
   WM8994_AIF1_DRC2_4         : constant Register_Address := 16#0454#;
   WM8994_AIF1_DAC1_EQG_1     : constant Register_Address := 16#0480#;
   WM8994_AIF1_DAC1_EQG_2     : constant Register_Address := 16#0481#;
   WM8994_AIF1_DAC1_EQG_1A    : constant Register_Address := 16#0482#;
   WM8994_AIF1_DAC1_EQG_1B    : constant Register_Address := 16#0483#;
   WM8994_AIF1_DAC1_EQG_1PG   : constant Register_Address := 16#0484#;
   WM8994_AIF1_DAC1_EQG_2A    : constant Register_Address := 16#0485#;
   WM8994_AIF1_DAC1_EQG_2B    : constant Register_Address := 16#0486#;
   WM8994_AIF1_DAC1_EQG_2C    : constant Register_Address := 16#0487#;
   WM8994_AIF1_DAC1_EQG_2PG   : constant Register_Address := 16#0488#;
   WM8994_AIF1_DAC1_EQG_3A    : constant Register_Address := 16#0489#;
   WM8994_AIF1_DAC1_EQG_3B    : constant Register_Address := 16#048A#;
   WM8994_AIF1_DAC1_EQG_3C    : constant Register_Address := 16#048B#;
   WM8994_AIF1_DAC1_EQG_3PG   : constant Register_Address := 16#048C#;
   WM8994_AIF1_DAC1_EQG_4A    : constant Register_Address := 16#048D#;
   WM8994_AIF1_DAC1_EQG_4B    : constant Register_Address := 16#048E#;
   WM8994_AIF1_DAC1_EQG_4C    : constant Register_Address := 16#048F#;
   WM8994_AIF1_DAC1_EQG_4PG   : constant Register_Address := 16#0490#;
   WM8994_AIF1_DAC1_EQG_5A    : constant Register_Address := 16#0491#;
   WM8994_AIF1_DAC1_EQG_5B    : constant Register_Address := 16#0492#;
   WM8994_AIF1_DAC1_EQG_5PG   : constant Register_Address := 16#0493#;
   WM8994_AIF1_DAC2_EQG_1     : constant Register_Address := 16#04A0#;
   WM8994_AIF1_DAC2_EQG_2     : constant Register_Address := 16#04A1#;
   WM8994_AIF1_DAC2_EQG_1A    : constant Register_Address := 16#04A2#;
   WM8994_AIF1_DAC2_EQG_1B    : constant Register_Address := 16#04A3#;
   WM8994_AIF1_DAC2_EQG_1PG   : constant Register_Address := 16#04A4#;
   WM8994_AIF1_DAC2_EQG_2A    : constant Register_Address := 16#04A5#;
   WM8994_AIF1_DAC2_EQG_2B    : constant Register_Address := 16#04A6#;
   WM8994_AIF1_DAC2_EQG_2C    : constant Register_Address := 16#04A7#;
   WM8994_AIF1_DAC2_EQG_2PG   : constant Register_Address := 16#04A8#;
   WM8994_AIF1_DAC2_EQG_3A    : constant Register_Address := 16#04A9#;
   WM8994_AIF1_DAC2_EQG_3B    : constant Register_Address := 16#04AA#;
   WM8994_AIF1_DAC2_EQG_3C    : constant Register_Address := 16#04AB#;
   WM8994_AIF1_DAC2_EQG_3PG   : constant Register_Address := 16#04AC#;
   WM8994_AIF1_DAC2_EQG_4A    : constant Register_Address := 16#04AD#;
   WM8994_AIF1_DAC2_EQG_4B    : constant Register_Address := 16#04AE#;
   WM8994_AIF1_DAC2_EQG_4C    : constant Register_Address := 16#04AF#;
   WM8994_AIF1_DAC2_EQG_4PG   : constant Register_Address := 16#04B0#;
   WM8994_AIF1_DAC2_EQG_5A    : constant Register_Address := 16#04B1#;
   WM8994_AIF1_DAC2_EQG_5B    : constant Register_Address := 16#04B2#;
   WM8994_AIF1_DAC2_EQG_5PG   : constant Register_Address := 16#04B3#;
   WM8994_AIF2_ADC_Left_Vol   : constant Register_Address := 16#0500#;
   WM8994_AIF2_ADC_Right_Vol  : constant Register_Address := 16#0501#;
   WM8994_AIF2_DAC_Left_Vol   : constant Register_Address := 16#0502#;
   WM8994_AIF2_DAC_Right_Vol  : constant Register_Address := 16#0503#;
   WM8994_AIF2_ADC_Filters    : constant Register_Address := 16#0510#;
   WM8994_AIF2_DAC_Filter_1   : constant Register_Address := 16#0520#;
   WM8994_AIF2_DAC_Filter_2   : constant Register_Address := 16#0521#;
   WM8994_AIF2_DRC_1          : constant Register_Address := 16#0540#;
   WM8994_AIF2_DRC_2          : constant Register_Address := 16#0541#;
   WM8994_AIF2_DRC_3          : constant Register_Address := 16#0542#;
   WM8994_AIF2_DRC_4          : constant Register_Address := 16#0543#;
   WM8994_AIF2_DRC_5          : constant Register_Address := 16#0544#;
   WM8994_AIF2_EQG_1          : constant Register_Address := 16#0580#;
   WM8994_AIF2_EQG_2          : constant Register_Address := 16#0581#;
   WM8994_AIF2_EQG_1A         : constant Register_Address := 16#0582#;
   WM8994_AIF2_EQG_1B         : constant Register_Address := 16#0583#;
   WM8994_AIF2_EQG_1PG        : constant Register_Address := 16#0584#;
   WM8994_AIF2_EQG_2A         : constant Register_Address := 16#0585#;
   WM8994_AIF2_EQG_2B         : constant Register_Address := 16#0586#;
   WM8994_AIF2_EQG_2C         : constant Register_Address := 16#0587#;
   WM8994_AIF2_EQG_2PG        : constant Register_Address := 16#0588#;
   WM8994_AIF2_EQG_3A         : constant Register_Address := 16#0589#;
   WM8994_AIF2_EQG_3B         : constant Register_Address := 16#058A#;
   WM8994_AIF2_EQG_3C         : constant Register_Address := 16#058B#;
   WM8994_AIF2_EQG_3PG        : constant Register_Address := 16#058C#;
   WM8994_AIF2_EQG_4A         : constant Register_Address := 16#058D#;
   WM8994_AIF2_EQG_4B         : constant Register_Address := 16#058E#;
   WM8994_AIF2_EQG_4C         : constant Register_Address := 16#058F#;
   WM8994_AIF2_EQG_4PG        : constant Register_Address := 16#0590#;
   WM8994_AIF2_EQG_5A         : constant Register_Address := 16#0591#;
   WM8994_AIF2_EQG_5B         : constant Register_Address := 16#0592#;
   WM8994_AIF2_EQG_5PG        : constant Register_Address := 16#0593#;
   WM8994_DAC1_Mixer_Vol      : constant Register_Address := 16#0600#;
   WM8994_AIF1_DAC1_LMR       : constant Register_Address := 16#0601#;
   WM8994_AIF1_DAC1_RMR       : constant Register_Address := 16#0602#;
   WM8994_DAC2_Mixer_Vol      : constant Register_Address := 16#0603#;
   WM8994_AIF1_DAC2_LMR       : constant Register_Address := 16#0604#;
   WM8994_AIF1_DAC2_RMR       : constant Register_Address := 16#0605#;
   WM8994_AIF1_ADC1_LMR       : constant Register_Address := 16#0606#;
   WM8994_AIF1_ADC1_RMR       : constant Register_Address := 16#0607#;
   WM8994_AIF1_ADC2_LMR       : constant Register_Address := 16#0608#;
   WM8994_AIF1_ADC2_RMR       : constant Register_Address := 16#0609#;
   WM8994_DAC1_Left_Vol       : constant Register_Address := 16#0610#;
   WM8994_DAC1_Right_Vol      : constant Register_Address := 16#0611#;
   WM8994_DAC2_Left_Vol       : constant Register_Address := 16#0612#;
   WM8994_DAC2_Right_Vol      : constant Register_Address := 16#0613#;
   WM8994_DAC_SoftMute        : constant Register_Address := 16#0614#;
   WM8994_Oversampling        : constant Register_Address := 16#0620#;
   WM8994_Sidetone            : constant Register_Address := 16#0621#;
   WM8994_GPIO1               : constant Register_Address := 16#0700#;
   WM8994_GPIO2               : constant Register_Address := 16#0701#;
   WM8994_GPIO3               : constant Register_Address := 16#0702#;
   WM8994_GPIO4               : constant Register_Address := 16#0703#;
   WM8994_GPIO5               : constant Register_Address := 16#0704#;
   WM8994_GPIO6               : constant Register_Address := 16#0705#;
   WM8994_GPIO7               : constant Register_Address := 16#0706#;
   WM8994_GPIO8               : constant Register_Address := 16#0707#;
   WM8994_GPIO9               : constant Register_Address := 16#0708#;
   WM8994_GPIO10              : constant Register_Address := 16#0709#;
   WM8994_GPIO11              : constant Register_Address := 16#070A#;
   WM8994_PULL_Control_1      : constant Register_Address := 16#0720#;
   WM8994_PULL_Control_2      : constant Register_Address := 16#0721#;
   WM8994_INT_Status_1        : constant Register_Address := 16#0730#;
   WM8994_INT_Status_2        : constant Register_Address := 16#0731#;
   WM8994_INT_Raw_Status_2    : constant Register_Address := 16#0732#;
   WM8994_INT_Status1_Mask    : constant Register_Address := 16#0738#;
   WM8994_INT_Status2_Mask    : constant Register_Address := 16#0739#;
   WM8994_INT_Control         : constant Register_Address := 16#0740#;
   WM8994_IRQ_Debounce        : constant Register_Address := 16#0748#;
   WM8994_WRITE_Sequencer0    : constant Register_Address := 16#3000#;
   WM8994_WRITE_Sequencer1    : constant Register_Address := 16#3001#;
   WM8994_WRITE_Sequencer2    : constant Register_Address := 16#3002#;
   WM8994_WRITE_Sequencer3    : constant Register_Address := 16#3003#;
   WM8994_WRITE_Sequencer4    : constant Register_Address := 16#3508#;
   WM8994_WRITE_Sequencer5    : constant Register_Address := 16#3509#;
   WM8994_WRITE_Sequencer6    : constant Register_Address := 16#3510#;
   WM8994_WRITE_Sequencer7    : constant Register_Address := 16#3511#;
   WM8994_SW_Reset_Mask       : constant Register_Address := 16#FFFF#;

end WM8994.IO;
