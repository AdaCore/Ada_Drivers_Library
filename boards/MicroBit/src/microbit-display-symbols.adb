------------------------------------------------------------------------------
--                                                                          --
--                       Copyright (C) 2019, AdaCore                        --
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

package body MicroBit.Display.Symbols is

   ----------------
   -- Left_Arrow --
   ----------------

   procedure Left_Arrow is
   begin
      Set (0, 2);
      Set (1, 2);
      Set (2, 2);
      Set (3, 2);
      Set (4, 2);
      Set (1, 2);

      Set (1, 1);
      Set (2, 0);
      Set (1, 3);
      Set (2, 4);
   end Left_Arrow;

   -----------------
   -- Right_Arrow --
   -----------------

   procedure Right_Arrow is
   begin
      Set (0, 2);
      Set (1, 2);
      Set (2, 2);
      Set (3, 2);
      Set (4, 2);

      Set (3, 1);
      Set (2, 0);
      Set (3, 3);
      Set (2, 4);
   end Right_Arrow;

   --------------
   -- Up_Arrow --
   --------------

   procedure Up_Arrow is
   begin
      Set (2, 0);
      Set (2, 1);
      Set (2, 2);
      Set (2, 3);
      Set (2, 4);

      Set (1, 1);
      Set (0, 2);
      Set (3, 1);
      Set (4, 2);
   end Up_Arrow;

   ----------------
   -- Down_Arrow --
   ----------------

   procedure Down_Arrow is
   begin
      Set (2, 0);
      Set (2, 1);
      Set (2, 2);
      Set (2, 3);
      Set (2, 4);

      Set (1, 3);
      Set (0, 2);
      Set (3, 3);
      Set (4, 2);
   end Down_Arrow;

   -----------
   -- Smile --
   -----------

   procedure Smile is
   begin
      Set (1, 1);
      Set (3, 1);
      Set (0, 3);
      Set (1, 4);
      Set (2, 4);
      Set (3, 4);
      Set (4, 3);
   end Smile;

   -----------
   -- Frown --
   -----------

   procedure Frown is
   begin
      Set (0, 4);
      Set (1, 1);
      Set (1, 3);
      Set (2, 3);
      Set (3, 1);
      Set (3, 3);
      Set (4, 4);
   end Frown;

   -----------
   -- Cross --
   -----------

   procedure Cross is
   begin
      Set (0, 0);
      Set (1, 1);
      Set (2, 2);
      Set (3, 3);
      Set (4, 4);
      Set (0, 4);
      Set (1, 3);
      Set (3, 1);
      Set (4, 0);
   end Cross;

   ---------------
   -- Checkmark --
   ---------------

   procedure Checkmark is
   begin
      Set (4, 1);
      Set (3, 2);
      Set (2, 3);
      Set (1, 4);
      Set (0, 3);
   end Checkmark;

   -----------
   -- Heart --
   -----------

   procedure Heart is
   begin
      Set (0, 1);
      Set (0, 2);
      Set (1, 0);
      Set (1, 3);
      Set (2, 1);
      Set (2, 4);
      Set (3, 0);
      Set (3, 3);
      Set (4, 1);
      Set (4, 2);
   end Heart;

end MicroBit.Display.Symbols;
