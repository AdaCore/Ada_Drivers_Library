--  Driver for the cs32l22 CODEC

with Interfaces; use Interfaces;
with HAL;        use HAL;
with HAL.I2C;    use HAL.I2C;

package cs32l22 is

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

   --  not sure what id this is
   --  fix me for cs32l22. This id is probably for w8994. I am not sure
   --  exactly because it.
   CS32L22_ID : constant := 16#8994#;

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

   subtype Volume_Level is Unsigned_8 range 0 .. 100;

   type cs32l22_Device
     (Port     : not null I2C_Port_Ref;
      I2C_Addr : UInt10)
   is tagged limited private;

   procedure Init (This      : in out cs32l22_Device;
                   Input     : Input_Device;
                   Output    : Output_Device;
                   Volume    : Unsigned_8;
                   Frequency : Audio_Frequency);

   function Read_ID (This : in out cs32l22_Device) return Unsigned_16;
   procedure Play (This : in out cs32l22_Device);
   procedure Pause (This : in out cs32l22_Device);
   procedure Resume (This : in out cs32l22_Device);
   procedure Stop (This : in out cs32l22_Device; Cmd : Stop_Mode);
   procedure Set_Volume (This : in out cs32l22_Device; Volume : Volume_Level);
   procedure Set_Mute (This : in out cs32l22_Device; Cmd : Mute);
   procedure Set_Output_Mode (This : in out cs32l22_Device;
                              Device : Output_Device);
   procedure Set_Frequency (This : in out cs32l22_Device;
                            Freq : Audio_Frequency);
   procedure Reset (This : in out cs32l22_Device);

private
   type cs32l22_Device (Port     : not null I2C_Port_Ref;
                       I2C_Addr : UInt10) is tagged limited null record;

   procedure I2C_Write (This   : in out cs32l22_Device;
                        Reg   : Short;
                        Value : Short);
   function I2C_Read (This : in out cs32l22_Device;
                      Reg : Short)
                      return Short;

end cs32l22;
