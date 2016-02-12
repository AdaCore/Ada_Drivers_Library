--  Generic spec for Audio drivers

package STM32.Audio is

   type Audio_Buffer is array (Natural range <>) of Short;

   type Audio_Frequency is
     (Audio_Freq_8kHz,
      Audio_Freq_11kHz,
      Audio_Freq_16kHz,
      Audio_Freq_22kHz,
      Audio_Freq_44kHz,
      Audio_Freq_48kHz,
      Audio_Freq_96kHz)
     with Size => 32;
   for Audio_Frequency use
     (Audio_Freq_8kHz  =>  8_000,
      Audio_Freq_11kHz => 11_025,
      Audio_Freq_16kHz => 16_000,
      Audio_Freq_22kHz => 22_050,
      Audio_Freq_44kHz => 44_100,
      Audio_Freq_48kHz => 48_000,
      Audio_Freq_96kHz => 96_000);


   procedure Initialize_Audio_Out
     (Volume    : Byte;
      Frequency : Audio_Frequency);

   procedure Play
     (Buffer : Audio_Buffer);

   procedure Change_Buffer
     (Buffer : Audio_Buffer);

   procedure Pause;

   procedure Resume;

   procedure Stop;

   procedure Set_Volume
     (Volume : Byte);

   procedure Set_Frequency
     (Frequency : Audio_Frequency);

end STM32.Audio;
