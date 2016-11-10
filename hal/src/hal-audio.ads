with Interfaces; use Interfaces;

package HAL.Audio is

   type Audio_Buffer is array (Natural range <>) of Integer_16
     with Component_Size => 16, Alignment => 4;

   type Audio_Volume is new Natural range 0 .. 100;

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

   type Audio_Buffer_Completion_Level is
     (Half, Full);

   type Audio_Device is limited interface;

   procedure Initialize_Audio_Out
     (This      : in out Audio_Device;
      Volume    : Audio_Volume;
      Frequency : Audio_Frequency) is abstract;

   procedure Play
     (This   : in out Audio_Device;
      Buffer : Audio_Buffer) is abstract;

   procedure Pause
     (This : in out Audio_Device) is abstract;

   procedure Resume
     (This : in out Audio_Device) is abstract;

   procedure Stop
     (This : in out Audio_Device) is abstract;

   procedure Set_Volume
     (This   : in out Audio_Device;
      Volume : Audio_Volume) is abstract;

   procedure Set_Frequency
     (This      : in out Audio_Device;
      Frequency : Audio_Frequency) is abstract;

end HAL.Audio;
