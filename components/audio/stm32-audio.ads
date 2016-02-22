--  Generic spec for Audio drivers

package STM32.Audio is

   type Audio_Buffer is array (Natural range <>) of Integer_16
     with Component_Size => 16, Alignment => 32;

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

   type DMA_Half_Complete_CB is access procedure;
   type DMA_Transfer_Complete_CB is access procedure;
   type DMA_Error_CB is access procedure (Err : DMA_Error);


   procedure Initialize_Audio_Out
     (Volume    : Byte;
      Frequency : Audio_Frequency);

   procedure Set_Half_Complete_Callback
     (Callback : DMA_Half_Complete_CB);

   procedure Set_Transfer_Complete_Callback
     (Callback : DMA_Transfer_Complete_CB);

   procedure Set_Error_Callback
     (Callback : DMA_Error_CB);

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
