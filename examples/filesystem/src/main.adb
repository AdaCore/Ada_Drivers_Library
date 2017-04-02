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


with HAL.Filesystem;        use HAL.Filesystem;
with Pathname_Manipulation; use Pathname_Manipulation;
with Ada.Text_IO;           use Ada.Text_IO;
with My_Stream;             use My_Stream;

procedure Main is

   procedure Pretty_Print (Path : Pathname; Parsed : Path_Delimiters);

   ------------------
   -- Pretty_Print --
   ------------------

   procedure Pretty_Print (Path : Pathname; Parsed : Path_Delimiters) is
      Block : Integer := Parsed'First;
   begin
      Put_Line ("Path : " & Path);
      if Parsed'Length = 0 then
         Put_Line ("No delimiters.");
         return;
      end if;

      Put ("       ");
      for Index in Path'Range loop
         if Index = Parsed (Block).From or else Index = Parsed (Block).To then
            Put ('|');
         else
            Put (' ');
         end if;
         if Index = Parsed (Block).To then
            if Block = Parsed'Last then
               exit;
            else
               Block := Block + 1;
            end if;
         end if;
      end loop;
      New_Line;

      for B of Parsed loop
         Put_Line ("Delim from:" & B.From'Img & " To:" & B.To'Img & " -> '" &
                     Path (B.From .. B.To) & "'");
      end loop;
   end Pretty_Print;

   Path1 : constant String := "/1/22/333/4444.txt";
   Path2 : constant String := "//";
   Path3 : constant String := "/a//b.txt";
   Path4 : constant String := "/x/y.txt/";

   Arf : aliased My_Stream.Lol;
   Test : Integer;
begin

   String'Write (Arf'Access, Path1);

   Integer'Read (Arf'Access, Test);

   Put_Line ("=== Path test ===");
   Pretty_Print (Path1, Parse (Path1));
   Pretty_Print (Path2, Parse (Path2));
   Pretty_Print (Path3, Parse (Path3));
   Pretty_Print (Path4, Parse (Path4));
end Main;
