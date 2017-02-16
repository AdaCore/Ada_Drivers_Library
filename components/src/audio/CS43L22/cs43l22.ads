------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2015-2016, AdaCore                     --
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

--  Driver for the WM8994 CODEC

with HAL;       use HAL;
with HAL.I2C;   use HAL.I2C;
with HAL.Audio; use HAL.Audio;
with HAL.Time;

package CS43L22 is

   type Output_Device is
     (No_Output,
      Speaker,
      Headphone,
      Both,
      Auto);

   CS43L22_I2C_Addr : constant := 16#94#;

   CS43L22_ID       : constant := 16#E0#;
   CS43L22_ID_MASK  : constant := 16#F8#;

   type Mute is
     (Mute_On,
      Mute_Off);

   subtype Volume_Level is UInt8 range 0 .. 100;

   type CS43L22_Device (Port : not null Any_I2C_Port;
                        Time : not null HAL.Time.Any_Delays) is
     tagged limited private;

   procedure Init (This      : in out CS43L22_Device;
                   Output    : Output_Device;
                   Volume    : Volume_Level;
                   Frequency : Audio_Frequency);

   function Read_ID (This : in out CS43L22_Device) return UInt8;
   procedure Play (This : in out CS43L22_Device);
   procedure Pause (This : in out CS43L22_Device);
   procedure Resume (This : in out CS43L22_Device);
   procedure Stop (This : in out CS43L22_Device);
   procedure Set_Volume (This : in out CS43L22_Device; Volume : Volume_Level);
   procedure Set_Mute (This : in out CS43L22_Device; Cmd : Mute);
   procedure Set_Output_Mode (This : in out CS43L22_Device;
                              Device : Output_Device);
   procedure Set_Frequency (This : in out CS43L22_Device;
                            Freq : Audio_Frequency);
   procedure Reset (This : in out CS43L22_Device);

private

   CS43L22_REG_ID                  : constant := 16#01#;
   CS43L22_REG_POWER_CTL1          : constant := 16#02#;
   CS43L22_REG_POWER_CTL2          : constant := 16#04#;
   CS43L22_REG_CLOCKING_CTL        : constant := 16#05#;
   CS43L22_REG_INTERFACE_CTL1      : constant := 16#06#;
   CS43L22_REG_INTERFACE_CTL2      : constant := 16#07#;
   CS43L22_REG_PASSTHR_A_SELECT    : constant := 16#08#;
   CS43L22_REG_PASSTHR_B_SELECT    : constant := 16#09#;
   CS43L22_REG_ANALOG_ZC_SR_SETT   : constant := 16#0A#;
   CS43L22_REG_PASSTHR_GANG_CTL    : constant := 16#0C#;
   CS43L22_REG_PLAYBACK_CTL1       : constant := 16#0D#;
   CS43L22_REG_MISC_CTL            : constant := 16#0E#;
   CS43L22_REG_PLAYBACK_CTL2       : constant := 16#0F#;
   CS43L22_REG_PASSTHR_A_VOL       : constant := 16#14#;
   CS43L22_REG_PASSTHR_B_VOL       : constant := 16#15#;
   CS43L22_REG_PCMA_VOL            : constant := 16#1A#;
   CS43L22_REG_PCMB_VOL            : constant := 16#1B#;
   CS43L22_REG_BEEP_FREQ_ON_TIME   : constant := 16#1C#;
   CS43L22_REG_BEEP_VOL_OFF_TIME   : constant := 16#1D#;
   CS43L22_REG_BEEP_TONE_CFG       : constant := 16#1E#;
   CS43L22_REG_TONE_CTL            : constant := 16#1F#;
   CS43L22_REG_MASTER_A_VOL        : constant := 16#20#;
   CS43L22_REG_MASTER_B_VOL        : constant := 16#21#;
   CS43L22_REG_HEADPHONE_A_VOL     : constant := 16#22#;
   CS43L22_REG_HEADPHONE_B_VOL     : constant := 16#23#;
   CS43L22_REG_SPEAKER_A_VOL       : constant := 16#24#;
   CS43L22_REG_SPEAKER_B_VOL       : constant := 16#25#;
   CS43L22_REG_CH_MIXER_SWAP       : constant := 16#26#;
   CS43L22_REG_LIMIT_CTL1          : constant := 16#27#;
   CS43L22_REG_LIMIT_CTL2          : constant := 16#28#;
   CS43L22_REG_LIMIT_ATTACK_RATE   : constant := 16#29#;
   CS43L22_REG_OVF_CLK_STATUS      : constant := 16#2E#;
   CS43L22_REG_BATT_COMPENSATION   : constant := 16#2F#;
   CS43L22_REG_VP_BATTERY_LEVEL    : constant := 16#30#;
   CS43L22_REG_SPEAKER_STATUS      : constant := 16#31#;
   CS43L22_REG_TEMPMONITOR_CTL     : constant := 16#32#;
   CS43L22_REG_THERMAL_FOLDBACK    : constant := 16#33#;
   CS43L22_REG_CHARGE_PUMP_FREQ    : constant := 16#34#;

   type CS43L22_Device (Port : not null Any_I2C_Port;
                        Time : not null HAL.Time.Any_Delays) is
     tagged limited record
      Output_Enabled : Boolean := False;
      Output_Dev     : UInt8 := 0;
   end record;

   procedure I2C_Write (This  : in out CS43L22_Device;
                        Reg   : UInt8;
                        Value : UInt8);
   function I2C_Read (This : in out CS43L22_Device;
                      Reg  : UInt8)
                      return UInt8;

end CS43L22;
