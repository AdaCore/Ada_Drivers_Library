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

package body CS43L22 is

   ---------------
   -- I2C_Write --
   ---------------

   procedure I2C_Write (This  : in out CS43L22_Device;
                        Reg   : UInt8;
                        Value : UInt8)
   is
      Status : I2C_Status;

   begin
      This.Port.Mem_Write
        (Addr          => CS43L22_I2C_Addr,
         Mem_Addr      => UInt16 (Reg),
         Mem_Addr_Size => Memory_Size_8b,
         Data          => (1 => Value),
         Status        => Status);
   end I2C_Write;

   --------------
   -- I2C_Read --
   --------------

   function I2C_Read (This : in out CS43L22_Device;
                      Reg : UInt8)
                      return UInt8
   is
      Status : I2C_Status;
      Data   : I2C_Data (1 .. 1);

   begin
      This.Port.Mem_Read
        (Addr          => CS43L22_I2C_Addr,
         Mem_Addr      => UInt16 (Reg),
         Mem_Addr_Size => Memory_Size_8b,
         Data          => Data,
         Status        => Status);
      return Data (1);
   end I2C_Read;

   ----------
   -- Init --
   ----------

   procedure Init
     (This      : in out CS43L22_Device;
      Output    : Output_Device;
      Volume    : Volume_Level;
      Frequency : Audio_Frequency)
   is
      pragma Unreferenced (Frequency);
   begin
      --  Codec set to power OFF
      This.I2C_Write (CS43L22_REG_POWER_CTL1, 16#01#);

      --  Save output device for mute ON/OFF procedure
      This.Set_Output_Mode (Output);

      --  Clock configuration: auto detection
      This.I2C_Write (CS43L22_REG_CLOCKING_CTL, 16#81#);

      --  Set the Slave Mode and the audio Standard
      This.I2C_Write (CS43L22_REG_INTERFACE_CTL1, 16#04#);

      --  Set the Master volume */
      This.Set_Volume (Volume);

      --  If the Speaker is enabled, set the Mono mode and volume attenuation
      --  level
      if (Output /= Headphone) then
         --  Set the Speaker Mono mode
         This.I2C_Write (CS43L22_REG_PLAYBACK_CTL2, 16#06#);

         --  Set the Speaker attenuation level
         This.I2C_Write (CS43L22_REG_SPEAKER_A_VOL, 16#00#);
         This.I2C_Write (CS43L22_REG_SPEAKER_B_VOL, 16#00#);
      end if;

      --  Additional configuration for the CODEC. These configurations are
      --  done to reduce the time needed for the Codec to power off. If these
      --  configurations are removed, then a long delay should be added between
      --  powering off the Codec and switching off the I2S peripheral MCLK
      --  clock (which is the operating clock for Codec).

      --  If this delay is not inserted, then the codec will not shut down
      --  properly and it results in high noise after shut down.

      --  Disable the analog soft ramp
      This.I2C_Write (CS43L22_REG_ANALOG_ZC_SR_SETT, 16#00#);
      --  Disable the digital soft ramp
      This.I2C_Write (CS43L22_REG_MISC_CTL, 16#04#);
      --  Disable the limiter attack level
      This.I2C_Write (CS43L22_REG_LIMIT_CTL1, 16#00#);
      --  Adjust Bass and Treble levels */
      This.I2C_Write (CS43L22_REG_TONE_CTL, 16#0F#);
      --  Adjust PCM volume level */
      This.I2C_Write (CS43L22_REG_PCMA_VOL, 16#0A#);
      This.I2C_Write (CS43L22_REG_PCMB_VOL, 16#0A#);
   end Init;

   -------------
   -- Read_ID --
   -------------

   function Read_ID (This : in out CS43L22_Device) return UInt8 is
   begin
      return This.I2C_Read (CS43L22_REG_ID) and CS43L22_ID_MASK;
   end Read_ID;

   ----------
   -- Play --
   ----------

   procedure Play (This : in out CS43L22_Device) is
   begin
      if not This.Output_Enabled then
         --  Enable the digital soft ramp
         This.I2C_Write (CS43L22_REG_MISC_CTL, 16#06#);

         --  Enable output device
         This.Set_Mute (Mute_Off);

         --  Power on the Codec
         This.I2C_Write (CS43L22_REG_POWER_CTL1, 16#9E#);

         This.Output_Enabled := True;
      end if;
   end Play;

   -----------
   -- Pause --
   -----------

   procedure Pause (This : in out CS43L22_Device) is
   begin
      --  Pause the audio playing
      This.Set_Mute (Mute_On);

      --  CODEC in powersave mode
      This.I2C_Write (CS43L22_REG_POWER_CTL1, 16#01#);
   end Pause;

   ------------
   -- Resume --
   ------------

   procedure Resume (This : in out CS43L22_Device) is
   begin
      --  Unmute the output first
      This.Set_Mute (Mute_Off);

      This.Time.Delay_Milliseconds (1);

      This.I2C_Write (CS43L22_REG_POWER_CTL2, This.Output_Dev);

      --  Exit the power save mode
      This.I2C_Write (CS43L22_REG_POWER_CTL1, 16#9E#);
   end Resume;

   ----------
   -- Stop --
   ----------

   procedure Stop (This : in out CS43L22_Device) is
   begin
      if This.Output_Enabled then
         --  Mute the output first
         This.Set_Mute (Mute_On);

         --  Disable the digital soft ramp
         This.I2C_Write (CS43L22_REG_MISC_CTL, 16#04#);

         --  Power down the DAC and the speaker
         This.I2C_Write (CS43L22_REG_POWER_CTL1, 16#9F#);

         This.Output_Enabled := False;
      end if;
   end Stop;

   ----------------
   -- Set_Volume --
   ----------------

   procedure Set_Volume (This : in out CS43L22_Device; Volume : Volume_Level)
   is
      --  Actual Volume in range 0 .. 16#3F#
      Converted_Volume : UInt8 :=
                           UInt8 (UInt16 (Volume) * 200 / 100);

   begin
      --  range goes the following:
      --  0 dB .. +12 dB:     coded from 0 to 24
      --  -102 dB .. -0.5 dB: coded from 52 to 255
      --  -102 dB:            applied for values in range 25 .. 52
      --  so we have a valid range of volume from -102 to +12 in steps of 0.5
      --  which means 225 significant values
      --  200 .. 224 for positive dB
      --  0 .. 199 for negative dB

      --  However positive values may lead to saturated output. We thus limit
      --  the volume to the -102 .. 0dB range

      if Converted_Volume >= 200 then
         Converted_Volume := Converted_Volume - 200;
      else
         Converted_Volume := Converted_Volume + 52;
      end if;

      This.I2C_Write (CS43L22_REG_MASTER_A_VOL, Converted_Volume);
      This.I2C_Write (CS43L22_REG_MASTER_B_VOL, Converted_Volume);
   end Set_Volume;

   --------------
   -- Set_Mute --
   --------------

   procedure Set_Mute (This : in out CS43L22_Device; Cmd : Mute) is
   begin
      if This.Output_Enabled then
         case Cmd is
            when Mute_On =>
               This.I2C_Write (CS43L22_REG_POWER_CTL2, 16#FF#);
               This.I2C_Write (CS43L22_REG_HEADPHONE_A_VOL, 16#01#);
               This.I2C_Write (CS43L22_REG_HEADPHONE_B_VOL, 16#01#);
            when Mute_Off =>
               This.I2C_Write (CS43L22_REG_HEADPHONE_A_VOL, 16#00#);
               This.I2C_Write (CS43L22_REG_HEADPHONE_B_VOL, 16#00#);
               This.I2C_Write (CS43L22_REG_POWER_CTL2, This.Output_Dev);
         end case;
      end if;
   end Set_Mute;

   ---------------------
   -- Set_Output_Mode --
   ---------------------

   procedure Set_Output_Mode (This   : in out CS43L22_Device;
                              Device : Output_Device)
   is
   begin
      case Device is
         when No_Output =>
            This.Output_Dev := 0;
         when Speaker =>
            This.Output_Dev := 16#FA#;
         when Headphone =>
            This.Output_Dev := 16#AF#;
         when Both =>
            This.Output_Dev := 16#AA#;
         when Auto =>
            This.Output_Dev := 16#05#;
      end case;

      This.I2C_Write (CS43L22_REG_POWER_CTL2, This.Output_Dev);
   end Set_Output_Mode;

   -------------------
   -- Set_Frequency --
   -------------------

   procedure Set_Frequency (This  : in out CS43L22_Device;
                            Freq : Audio_Frequency)
   is
      pragma Unreferenced (This, Freq);
   begin
      --  CODEC is automatically detecting the frequency. No need to do
      --  anything here.
      null;
   end Set_Frequency;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : in out CS43L22_Device)
   is
      pragma Unreferenced (This);
   begin
      null;
   end Reset;

end CS43L22;
