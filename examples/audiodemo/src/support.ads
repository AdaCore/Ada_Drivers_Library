with STM32.Audio; use STM32.Audio;

package Support is

   Buff  : Audio_Buffer (1 .. 48000); --  1 second of audio data

   procedure Check_Msg;

   procedure On_Half_Complete;

   procedure On_Complete;

   procedure On_Error (Err : DMA_Error);

end Support;
