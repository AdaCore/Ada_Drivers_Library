with Last_Chance_Handler;
pragma Unreferenced (Last_Chance_Handler);

with Cortex_M.Cache;

with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

with Interfaces;  use Interfaces;
with STM32.Audio; use STM32.Audio;
with LCD_Std_Out;
with BMP_Fonts;

with Support;     use Support;

procedure Demo is
   use Ada.Numerics;
   Freq  : constant Float := 48000.0; --  Sampling frequency
   PitchL : constant Float := 440.0;   --  targetted pitch: A 440Hz
   PitchR : constant Float := 220.0;   --  targetted pitch: A 220Hz
   Val    : Float;
   Time   : Float;

begin
   LCD_Std_Out.Set_Font (BMP_Fonts.Font8x8);
   LCD_Std_Out.Put_Line ("Init audio buffer...");
   for J in Buff'Range loop
      Time := Float (J / 2) / Freq;

      if J mod 2 = 0 then
         --  Target is Pitch rate with buffer running at Freq
         Val := Time * PitchR;
      else
         --  Target is Pitch rate with buffer running at Freq
         Val := Time * PitchL;
      end if;

      --  We keep the decimals: the progress of the sinusoid to generate
      Val := Val - Float'Floor (Val);
      --  New get the sinus value
      Val := Sin (Val * 2.0 * Pi);
      --  And apply the maximum level
      Buff (J) := Integer_16 (Val * 32767.0);
   end loop;

   --  Make sure the values are in memory before starting the DMA transfer
   Cortex_M.Cache.Clean_DCache
     (Buff (Buff'First)'Address, Buff (Buff'Last)'Address);

   LCD_Std_Out.Put_Line ("Init audio driver...");
   STM32.Audio.Initialize_Audio_Out (10, Audio_Freq_48kHz);
   STM32.Audio.Set_Half_Complete_Callback (Support.On_Half_Complete'Access);
   STM32.Audio.Set_Transfer_Complete_Callback (Support.On_Complete'Access);
   STM32.Audio.Set_Error_Callback (Support.On_Error'Access);
   LCD_Std_Out.Put_Line ("Start playing...");
   STM32.Audio.Pause;
   STM32.Audio.Play (Buff);

   loop
      Support.Check_Msg;
   end loop;

end Demo;
