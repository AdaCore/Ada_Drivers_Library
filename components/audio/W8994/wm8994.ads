--  Driver for the WM8994 CODEC

with STM32; use STM32;

generic

   with procedure IO_Write (Reg : Half_Word; Value : Half_Word);
   with function  IO_Read  (Reg : Half_Word) return Half_Word;

package WM8994 is

   type Output_Device is
     (No_Output,
      Speaker,
      Headphone,
      Both,
      Auto);
   type Input_Device is
     (No_Input,
      Microphone,
      Input_Line);

   WM8994_ID               : constant := 16#8994#;

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

   type Mute is
     (Mute_On,
      Mute_Off);

   type Stop_Mode is
     (Stop_Power_Down_Sw,
      Stop_Power_Down_Hw);
   --  Stop_Power_Down_Sw:
   --  only mutes the audio codec. When resuming from this mode the codec
   --  keeps the previous initialization (no need to re-Initialize the codec
   --  registers).
   --  Stop_Power_Down_Hw:
   --  Physically power down the codec. When resuming from this mode, the codec
   --  is set to default configuration (user should re-Initialize the codec in
   --  order to play again the audio stream).

   procedure Init (Input     : Input_Device;
                   Output    : Output_Device;
                   Volume    : Byte;
                   Frequency : Audio_Frequency);

   function Read_ID return Half_Word;
   procedure Play;
   procedure Pause;
   procedure Resume;
   procedure Stop (Cmd : Stop_Mode);
   procedure Set_Volume (Volume : Byte);
   procedure Set_Mute (Cmd : Mute);
   procedure Set_Output_Mode (Device : Output_Device);
   procedure Set_Frequency (Freq : Audio_Frequency);
   procedure Reset;

end WM8994;
