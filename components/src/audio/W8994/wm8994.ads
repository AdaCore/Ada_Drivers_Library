--  Driver for the WM8994 CODEC

with Interfaces; use Interfaces;
with HAL;        use HAL;
with HAL.I2C;    use HAL.I2C;
with HAL.Time;

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

   WM8994_ID : constant := 16#8994#;

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

   type WM8994_Device
     (Port     : not null I2C_Port_Ref;
      I2C_Addr : UInt10;
      Time     : not null HAL.Time.Delays_Ref)
   is tagged limited private;

   procedure Init (This      : in out WM8994_Device;
                   Input     : Input_Device;
                   Output    : Output_Device;
                   Volume    : Unsigned_8;
                   Frequency : Audio_Frequency);

   function Read_ID (This : in out WM8994_Device) return Unsigned_16;
   procedure Play (This : in out WM8994_Device);
   procedure Pause (This : in out WM8994_Device);
   procedure Resume (This : in out WM8994_Device);
   procedure Stop (This : in out WM8994_Device; Cmd : Stop_Mode);
   procedure Set_Volume (This : in out WM8994_Device; Volume : Volume_Level);
   procedure Set_Mute (This : in out WM8994_Device; Cmd : Mute);
   procedure Set_Output_Mode (This : in out WM8994_Device;
                              Device : Output_Device);
   procedure Set_Frequency (This : in out WM8994_Device;
                            Freq : Audio_Frequency);
   procedure Reset (This : in out WM8994_Device);

private
   type WM8994_Device (Port     : not null I2C_Port_Ref;
                       I2C_Addr : UInt10;
                       Time     : not null HAL.Time.Delays_Ref) is tagged limited null record;

   procedure I2C_Write (This   : in out WM8994_Device;
                        Reg   : UInt16;
                        Value : UInt16);
   function I2C_Read (This : in out WM8994_Device;
                      Reg : UInt16)
                      return UInt16;

end WM8994;
