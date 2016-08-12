--  Generic spec for Audio drivers

--  /!\ This is work in progress and not at a real Hardware Abstraction Layer

with Ada.Interrupts.Names;
with Interfaces;  use Interfaces;
with STM32.DMA;

package HAL.Audio is

   Audio_Out_DMA_Interrupt : Ada.Interrupts.Interrupt_ID renames
                               Ada.Interrupts.Names.DMA2_Stream4_Interrupt;

   type Audio_Buffer is array (Natural range <>) of Integer_16
     with Component_Size => 16, Alignment => 32;

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

   type DMA_Error is
     (FIFO_Error,
      Direct_Mode_Error,
      Transfer_Error);

   procedure Initialize_Audio_Out
     (Volume    : Audio_Volume;
      Frequency : Audio_Frequency);

   function DMA_Out_Status
     (Flag : STM32.DMA.DMA_Status_Flag) return Boolean;

   procedure DMA_Out_Clear_Status
     (Flag : STM32.DMA.DMA_Status_Flag);

   procedure Play
     (Buffer : Audio_Buffer);

   procedure Change_Buffer
     (Buffer : Audio_Buffer);

   procedure Pause;

   procedure Resume;

   procedure Stop;

   procedure Set_Volume
     (Volume : Audio_Volume);

   procedure Set_Frequency
     (Frequency : Audio_Frequency);

end HAL.Audio;
