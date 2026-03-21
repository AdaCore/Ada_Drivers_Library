------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2015-2026, AdaCore                     --
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

--  This package provides a simple driver for the WM8994 CODEC. It does not
--  provide a full definition of the WM8994's functionality.

with HAL;      use HAL;
with HAL.I2C;  use HAL.I2C;
with HAL.Time;

package WM8994 is

   type Audio_CODEC
     (Port     : not null Any_I2C_Port;
      I2C_Addr : UInt10;
      Time     : not null HAL.Time.Any_Delays)
   is tagged limited private;

   type Analog_Outputs is
     (No_Output,
      Speaker,
      Headphone,
      Both,
      Auto);

   type Input_Device is
     (No_Input,
      Microphone,
      Input_Line);

   type Audio_Frequency is
     (Audio_Freq_8kHz,
      Audio_Freq_11kHz,
      Audio_Freq_12kHz,
      Audio_Freq_16kHz,
      Audio_Freq_22kHz,
      Audio_Freq_24kHz,
      Audio_Freq_32kHz,
      Audio_Freq_44kHz,
      Audio_Freq_48kHz,
      Audio_Freq_88kHz,
      Audio_Freq_96kHz)
     with Size => 32;
   --  Sample rates from 8kHz to 96kHz are all supported, per Datasheet
   --  WM8994_Rev4.6, pages 43 and 93, from Cirrus Logic. See all Tables 37
   --  and 41.
   --
   --  Note that 88.2kHz and 96kHz modes are supported for AIF1 input (DAC
   --  playback) only.

   for Audio_Frequency use
     (Audio_Freq_8kHz  =>  8_000,
      Audio_Freq_11kHz => 11_025,
      Audio_Freq_12kHz => 12_000,
      Audio_Freq_16kHz => 16_000,
      Audio_Freq_22kHz => 22_050,
      Audio_Freq_24kHz => 24_000,
      Audio_Freq_32kHz => 32_000,
      Audio_Freq_44kHz => 44_100,
      Audio_Freq_48kHz => 48_000,
      Audio_Freq_88kHz => 88_200,
      Audio_Freq_96kHz => 96_000);

   Max_Volume : constant := 16#3F#;

   subtype Volume_Level is UInt16 range 0 .. Max_Volume;

   procedure Initialize
     (This      : in out Audio_CODEC;
      Input     : Input_Device;
      Output    : Analog_Outputs;
      Volume    : Volume_Level;
      Frequency : Audio_Frequency);

   type Mute_Modes is
     (Mute_On,
      Mute_Off);

   type Stop_Mode is
     (Stop_Power_Down_Sw,
      --  Stop_Power_Down_Sw only mutes the audio codec, it does not alter
      --  hardware settings. When resuming from this mode the codec keeps the
      --  previous initialization so there is no need to re-initialize the
      --  codec registers.
      Stop_Power_Down_Hw);
      --  Stop_Power_Down_Hw physically powers down the codec hardware. When
      --  resuming from this mode, the codec is set to default configuration
      --  so users should re-initialize the codec.

   WM8994_ID : constant := 16#8994#;

   function Chip_ID (This : in out Audio_CODEC) return UInt16;

   procedure Play (This : in out Audio_CODEC);

   procedure Pause (This : in out Audio_CODEC);

   procedure Resume (This : in out Audio_CODEC);

   procedure Stop (This : in out Audio_CODEC; Cmd : Stop_Mode);

   procedure Set_Volume (This : in out Audio_CODEC; Volume : Volume_Level);

   procedure Set_Mute (This : in out Audio_CODEC; Cmd : Mute_Modes);

   procedure Set_Output_Mode (This : in out Audio_CODEC;
                              Device : Analog_Outputs);

   procedure Set_Frequency (This : in out Audio_CODEC;
                            Freq : Audio_Frequency);

   procedure Reset (This : in out Audio_CODEC);

private

   type Audio_CODEC
     (Port     : not null Any_I2C_Port;
      I2C_Addr : UInt10;
      Time     : not null HAL.Time.Any_Delays)
   is tagged limited null record;

end WM8994;
