------------------------------------------------------------------------------
--                                                                          --
--                        Copyright (C) 2016, AdaCore                       --
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

--  This package provides a simple software synthesizer that can be used to try
--  audio streams and DACs.

with HAL.Audio;  use HAL.Audio;

package Simple_Synthesizer is

   type Synthesizer (Stereo    : Boolean := True;
                     Amplitude : Natural := 500) is
     limited new Audio_Stream with private;

   procedure Set_Note_Frequency (This : in out Synthesizer;
                                 Note : Float);

   overriding
   procedure Set_Frequency (This      : in out Synthesizer;
                            Frequency : Audio_Frequency);

   overriding
   procedure Transmit (This : in out Synthesizer;
                       Data : Audio_Buffer);

   overriding
   procedure Receive (This : in out Synthesizer;
                      Data : out Audio_Buffer);
private
   type Synthesizer (Stereo    : Boolean := True;
                     Amplitude : Natural := 500) is
     limited new Audio_Stream with record
      Frequency   : Audio_Frequency;
      Note        : Float := 0.0;
      Last_Sample : Float := 0.0;
   end record;
end Simple_Synthesizer;
