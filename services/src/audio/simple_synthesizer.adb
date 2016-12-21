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

with Interfaces; use Interfaces;

package body Simple_Synthesizer is

   ------------------------
   -- Set_Note_Frequency --
   ------------------------

   procedure Set_Note_Frequency
     (This : in out Synthesizer;
      Note : Float)
   is
   begin
      This.Note := Note;
   end Set_Note_Frequency;

   -------------------
   -- Set_Frequency --
   -------------------

   overriding procedure Set_Frequency
     (This      : in out Synthesizer;
      Frequency : Audio_Frequency)
   is
   begin
      This.Frequency := Frequency;
   end Set_Frequency;

   --------------
   -- Transmit --
   --------------

   overriding procedure Transmit
     (This : in out Synthesizer;
      Data : Audio_Buffer)
   is
   begin
      raise Program_Error with "This Synthesizer doesn't take input";
   end Transmit;

   -------------
   -- Receive --
   -------------

   overriding procedure Receive
     (This : in out Synthesizer;
      Data : out Audio_Buffer)
   is
      function Next_Sample return Integer_16;

      Rate      : constant Float :=
        Float (Audio_Frequency'Enum_Rep (This.Frequency));
      Note      : constant Float := This.Note;
      Amplitude : constant Float := Float (This.Amplitude);
      Delt      : constant Float := 2.0 / (Rate * (1.0 / Note));


      -----------------
      -- Next_Sample --
      -----------------

      function Next_Sample return Integer_16 is
      begin
         This.Last_Sample := This.Last_Sample + Delt;
         if This.Last_Sample >  1.0 then
            This.Last_Sample := -1.0;
         end if;
         return Integer_16 (This.Last_Sample * Amplitude);
      end Next_Sample;

      Tmp : Integer_16;
   begin
      if This.Stereo then
         if Data'Length mod 2 /= 0 then
            raise Program_Error with
              "Audio buffer for stereo output should have even length";
         end if;

         for Index in 0 .. (Data'Length / 2) - 1 loop
            Tmp := Next_Sample;
            Data ((Index * 2) + Data'First) := Tmp;
            Data ((Index * 2) + Data'First + 1) := Tmp;
         end loop;
      else
         for Elt of Data loop
            Elt := Next_Sample;
         end loop;
      end if;
   end Receive;

end Simple_Synthesizer;
