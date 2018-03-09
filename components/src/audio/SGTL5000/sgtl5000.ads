------------------------------------------------------------------------------
--                                                                          --
--                        Copyright (C) 2017, AdaCore                       --
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

with HAL;      use HAL;
with HAL.I2C;  use HAL.I2C;
with HAL.Time;

package SGTL5000 is

   type SGTL5000_DAC (Port : not null Any_I2C_Port;
                      Time : not null HAL.Time.Any_Delays) is
      tagged limited private;

   function Valid_Id (This : SGTL5000_DAC) return Boolean;

   --  Glossary:
   --  DAP : Digital Audio Processing
   --  ZCD : Zero Cross Detector

   -------------
   -- Volumes --
   -------------

   subtype DAC_Volume is UInt8 range 16#3C# .. 16#F0#;
   --  16#3C# => 0 dB
   --  16#3D# => -0.5 dB
   --  ...
   --  16#F0# => -90 dB

   procedure Set_DAC_Volume (This        : in out SGTL5000_DAC;
                             Left, Right : DAC_Volume);

   subtype ADC_Volume is UInt4;
   --  16#0# => 0 dB
   --  16#1# => +1.5 dB
   --  ...
   --  16#F# => +22.5 dB

   procedure Set_ADC_Volume (This        : in out SGTL5000_DAC;
                             Left, Right : ADC_Volume;
                             Minus_6db   : Boolean);

   subtype HP_Volume is UInt7;
   --  16#00# => +12 dB
   --  16#01# => +11.5 dB
   --  16#18# => 0 dB
   --  ...
   --  16#7F# => -51.5 dB

   procedure Set_Headphones_Volume (This        : in out SGTL5000_DAC;
                                    Left, Right : HP_Volume);

   subtype Line_Out_Volume is UInt5;

   procedure Set_Line_Out_Volume (This        : in out SGTL5000_DAC;
                                  Left, Right : Line_Out_Volume);

   procedure Mute_Headphones (This : in out SGTL5000_DAC;
                              Mute : Boolean := True);

   procedure Mute_Line_Out (This : in out SGTL5000_DAC;
                            Mute : Boolean := True);

   procedure Mute_ADC (This : in out SGTL5000_DAC;
                       Mute : Boolean := True);

   procedure Mute_DAC (This : in out SGTL5000_DAC;
                       Mute : Boolean := True);

   -------------------
   -- Power control --
   -------------------

   type Power_State is (On, Off);

   procedure Set_Power_Control (This    : in out SGTL5000_DAC;
                                ADC     : Power_State;
                                DAC     : Power_State;
                                DAP     : Power_State;
                                I2S_Out : Power_State;
                                I2S_In  : Power_State);

   type Analog_Ground_Voltage is new UInt5;
   --  25mv steps
   --  16#00# => 0.800 V
   --  16#1F# => 1.575 V

   type Current_Bias is new UInt3;
   --  16#0# => Nominal
   --  16#1#-16#3# => +12.5%
   --  16#4# => -12.5%
   --  16#5# => -25%
   --  16#6# => -37.5%
   --  16#7# => -50%

   procedure Set_Reference_Control (This          : in out SGTL5000_DAC;
                                    VAG           : Analog_Ground_Voltage;
                                    Bias          : Current_Bias;
                                    Slow_VAG_Ramp : Boolean);

   type Short_Detector_Level is new UInt3;
   --  16#3# => 25 mA
   --  16#2# => 50 mA
   --  16#1# => 75 mA
   --  16#0# => 100 mA
   --  16#4# => 125 mA
   --  16#5# => 150 mA
   --  16#6# => 175 mA
   --  16#7# => 200 mA

   procedure Set_Short_Detectors (This      : in out SGTL5000_DAC;
                                  Right_HP  : Short_Detector_Level;
                                  Left_HP   : Short_Detector_Level;
                                  Center_HP : Short_Detector_Level;
                                  Mode_LR   : UInt2;
                                  Mode_CM   : UInt2);

   type Linear_Regulator_Out_Voltage is new UInt4;
   --  16#0# => 1.60
   --  16#F# => 0.85

   type Charge_Pump_Source is (VDDA, VDDIO);

   procedure Set_Linereg_Control (This                     : in out SGTL5000_DAC;
                                  Charge_Pump_Src_Override : Boolean;
                                  Charge_Pump_Src          : Charge_Pump_Source;
                                  Linereg_Out_Voltage      : Linear_Regulator_Out_Voltage);

   procedure Set_Analog_Power (This                      : in out SGTL5000_DAC;
                               DAC_Mono                  : Boolean := False;
                               Linreg_Simple_PowerUp     : Boolean := False;
                               Startup_PowerUp           : Boolean := False;
                               VDDC_Charge_Pump_PowerUp  : Boolean := False;
                               PLL_PowerUp               : Boolean := False;
                               Linereg_D_PowerUp         : Boolean := False;
                               VCOAmp_PowerUp            : Boolean := False;
                               VAG_PowerUp               : Boolean := False;
                               ADC_Mono                  : Boolean := False;
                               Reftop_PowerUp            : Boolean := False;
                               Headphone_PowerUp         : Boolean := False;
                               DAC_PowerUp               : Boolean := False;
                               Capless_Headphone_PowerUp : Boolean := False;
                               ADC_PowerUp               : Boolean := False;
                               Linout_PowerUp            : Boolean := False);
   -----------------
   -- I2S control --
   -----------------

   type Rate_Mode is (SYS_FS,
                      Half_SYS_FS,
                      Quarter_SYS_FS,
                      Sixth_SYS_FS);

   type SYS_FS_Freq is (SYS_FS_32kHz,
                        SYS_FS_44kHz,
                        SYS_FS_48kHz,
                        SYS_FS_96kHz);

   type MCLK_Mode is (MCLK_256FS,
                      MCLK_384FS,
                      MCLK_512FS,
                      Use_PLL);

   procedure Set_Clock_Control (This : in out SGTL5000_DAC;
                                Rate : Rate_Mode;
                                FS   : SYS_FS_Freq;
                                MCLK : MCLK_Mode);

   type SCLKFREQ_Mode is (SCLKFREQ_64FS,
                          SCLKFREQ_32FS);

   type Data_Len_Mode is (Data_32b,
                          Data_24b,
                          Data_20b,
                          Data_16b);

   type I2S_Mode is (I2S_Left_Justified,
                     Right_Justified,
                     PCM);

   procedure Set_I2S_Control (This        : in out SGTL5000_DAC;
                              SCLKFREQ    : SCLKFREQ_Mode;
                              Invert_SCLK : Boolean;
                              Master_Mode : Boolean;
                              Data_Len    : Data_Len_Mode;
                              I2S         : I2S_Mode;
                              LR_Align    : Boolean;
                              LR_Polarity : Boolean);

   ------------------
   -- Audio Switch --
   ------------------


   type ADC_Source is (Microphone, Line_In);
   type DAP_Source is (ADC, I2S_In);
   type DAP_Mix_Source is (ADC, I2S_In);
   type DAC_Source is (ADC, I2S_In, DAP);
   type HP_Source is (Line_In, DAC);
   type I2S_Out_Source is (ADC, I2S_In, DAP);

   procedure Select_ADC_Source (This       : in out SGTL5000_DAC;
                                Source     : ADC_Source;
                                Enable_ZCD : Boolean);

   procedure Select_DAP_Source (This   : in out SGTL5000_DAC;
                                Source : DAP_Source);

   procedure Select_DAP_Mix_Source (This   : in out SGTL5000_DAC;
                                    Source : DAP_Mix_Source);

   procedure Select_DAC_Source (This   : in out SGTL5000_DAC;
                                Source : DAC_Source);

   procedure Select_HP_Source (This       : in out SGTL5000_DAC;
                               Source     : HP_Source;
                               Enable_ZCD : Boolean);

   procedure Select_I2S_Out_Source (This   : in out SGTL5000_DAC;
                                    Source : I2S_Out_Source);

private

   SGTL5000_Chip_ID : constant := 16#A000#;

   SGTL5000_QFN20_I2C_Addr : constant I2C_Address := 20;

   type SGTL5000_DAC (Port : not null Any_I2C_Port;
                      Time : not null HAL.Time.Any_Delays) is
     tagged limited null record;

   procedure I2C_Write (This  : in out SGTL5000_DAC;
                        Reg   : UInt16;
                        Value : UInt16);
   function I2C_Read (This : SGTL5000_DAC;
                      Reg  : UInt16)
                      return UInt16;

   procedure Modify (This  : in out SGTL5000_DAC;
                     Reg   : UInt16;
                     Mask  : UInt16;
                     Value : UInt16);

   ------------------------
   -- Register addresses --
   ------------------------

   Chip_ID_Reg                   : constant := 16#0000#;
   DIG_POWER_REG                 : constant := 16#0002#;
   CLK_CTRL_REG                  : constant := 16#0004#;
   I2S_CTRL_REG                  : constant := 16#0006#;
   SSS_CTRL_REG                  : constant := 16#000A#;
   ADCDAC_CTRL_REG               : constant := 16#000E#;
   DAC_VOL_REG                   : constant := 16#0010#;
   PAD_STRENGTH_REG              : constant := 16#0014#;
   ANA_ADC_CTRL_REG              : constant := 16#0020#;
   ANA_HP_CTRL_REG               : constant := 16#0022#;
   ANA_CTRL_REG                  : constant := 16#0024#;
   LINREG_CTRL_REG               : constant := 16#0026#;
   REF_CTRL_REG                  : constant := 16#0028#;
   MIC_CTRL_REG                  : constant := 16#002A#;
   LINE_OUT_CTRL_REG             : constant := 16#002C#;
   LINE_OUT_VOL_REG              : constant := 16#002E#;
   ANA_POWER_REG                 : constant := 16#0030#;
   PLL_CTRL_REG                  : constant := 16#0032#;
   CLK_TOP_CTRL_REG              : constant := 16#0034#;
   ANA_STATUS_REG                : constant := 16#0036#;
   ANA_TEST1_REG                 : constant := 16#0038#;
   ANA_TEST2_REG                 : constant := 16#003A#;
   SHORT_CTRL_REG                : constant := 16#003C#;
   DAP_CONTROL_REG               : constant := 16#0100#;
   DAP_PEQ_REG                   : constant := 16#0102#;
   DAP_BASS_ENHANCE_REG          : constant := 16#0104#;
   DAP_BASS_ENHANCE_CTRL_REG     : constant := 16#0106#;
   DAP_AUDIO_EQ_REG              : constant := 16#0108#;
   DAP_SGTL_SURROUND_REG         : constant := 16#010A#;
   DAP_FILTER_COEF_ACCESS_REG    : constant := 16#010C#;
   DAP_COEF_WR_B0_MSB_REG        : constant := 16#010E#;
   DAP_COEF_WR_B0_LSB_REG        : constant := 16#0110#;
   DAP_AUDIO_EQ_BASS_BAND0_REG   : constant := 16#0116#;
   DAP_AUDIO_EQ_BAND1_REG        : constant := 16#0118#;
   DAP_AUDIO_EQ_BAND2_REG        : constant := 16#011A#;
   DAP_AUDIO_EQ_BAND3_REG        : constant := 16#011C#;
   DAP_AUDIO_EQ_TREBLE_BAND4_REG : constant := 16#011E#;
   DAP_MAIN_CHAN_REG             : constant := 16#0120#;
   DAP_MIX_CHAN_REG              : constant := 16#0122#;
   DAP_AVC_CTRL_REG              : constant := 16#0124#;
   DAP_AVC_THRESHOLD_REG         : constant := 16#0126#;
   DAP_AVC_ATTACK_REG            : constant := 16#0128#;
   DAP_AVC_DECAY_REG             : constant := 16#012A#;
   DAP_COEF_WR_B1_MSB_REG        : constant := 16#012C#;
   DAP_COEF_WR_B1_LSB_REG        : constant := 16#012E#;
   DAP_COEF_WR_B2_MSB_REG        : constant := 16#0130#;
   DAP_COEF_WR_B2_LSB_REG        : constant := 16#0132#;
   DAP_COEF_WR_A1_MSB_REG        : constant := 16#0134#;
   DAP_COEF_WR_A1_LSB_REG        : constant := 16#0136#;
   DAP_COEF_WR_A2_MSB_REG        : constant := 16#0138#;
   DAP_COEF_WR_A2_LSB_REG        : constant := 16#013A#;

end SGTL5000;
