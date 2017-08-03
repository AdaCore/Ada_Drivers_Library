------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2015-2016, AdaCore                     --
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

package body Pathname_Manipulation is

   --------------
   -- Root_Dir --
   --------------

   procedure Root_Dir (Path        :     String;
                       Start, Stop : out Integer)
   is
   begin
      Start := Path'First;
      Stop := Start;
      if Path'Length = 0 then
         return;
      end if;

      while Start <= Path'Last and then Path (Start) = '/' loop
         Start := Start + 1;
      end loop;

      Stop := Start;
      while Stop + 1 <= Path'Last and then Path (Stop + 1) /= '/' loop
         Stop := Stop + 1;
      end loop;
   end Root_Dir;

   --------------
   -- Root_Dir --
   --------------

   function Root_Dir (Path : String) return String is
      Start, Stop : Integer;
   begin
      Root_Dir (Path, Start, Stop);
      if Start not in Path'Range or else Stop not in Path'Range then
         return "";
      else
         return Path (Start .. Stop);
      end if;
   end Root_Dir;

end Pathname_Manipulation;
