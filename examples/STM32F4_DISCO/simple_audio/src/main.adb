------------------------------------------------------------------------------
--                                                                          --
--                        Copyright (C) 2017, AdaCore                       --
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

--  This is a demo of the features available on the STM32F4-DISCOVERY board.
--
--  Press the blue button to change the note of the sound played in the
--  headphone jack. Press the black button to reset.

with HAL;                  use HAL;
with STM32.Device;         use STM32.Device;
with STM32.Board;          use STM32.Board;
with HAL.Audio;            use HAL.Audio;
with Simple_Synthesizer;
with Audio_Stream;         use Audio_Stream;
with System;               use System;
with Interfaces;           use Interfaces;
with STM32.User_Button;

procedure Main is

   Synth : Simple_Synthesizer.Synthesizer
     (Stereo    => True,
      Amplitude => Natural (Integer_16'Last / 3));
   Audio_Data_0 : Audio_Buffer (1 .. 64);
   Audio_Data_1 : Audio_Buffer (1 .. 64);
   Note : Float := 110.0;
begin
   Initialize_LEDs;

   Initialize_Audio;
   STM32.User_Button.Initialize;


   Synth.Set_Frequency (STM32.Board.Audio_Rate);
   STM32.Board.Audio_DAC.Set_Volume (60);

   STM32.Board.Audio_DAC.Play;

   Audio_TX_DMA_Int.Start (Destination => STM32.Board.Audio_I2S.Data_Register_Address,
                           Source_0    => Audio_Data_0'Address,
                           Source_1    => Audio_Data_1'Address,
                           Data_Count  => Audio_Data_0'Length);

   loop
      if STM32.User_Button.Has_Been_Pressed then
         Note := Note * 2.0;
         if Note > 880.0 then
            Note := 110.0;
         end if;
      end if;

      Synth.Set_Note_Frequency (Note);
      Audio_TX_DMA_Int.Wait_For_Transfer_Complete;

      if Audio_TX_DMA_Int.Not_In_Transfer = Audio_Data_0'Address then
         Synth.Receive (Audio_Data_0);
      else
         Synth.Receive (Audio_Data_1);
      end if;

   end loop;
end Main;
