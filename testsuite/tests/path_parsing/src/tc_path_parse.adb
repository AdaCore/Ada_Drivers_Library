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

with HAL.Filesystem;        use HAL.Filesystem;
with Pathname_Manipulation; use Pathname_Manipulation;
with Ada.Text_IO;
use Ada.Text_IO;

procedure TC_Path_Parse is
   pragma Assertion_Policy (Assert => Check);

   Path1 : constant String := "/1/22/333/4444.txt";
   Path2 : constant String := "////////////////";
   Path3 : constant String := "/a////b.txt";
   Path4 : constant String := "x/y.txt/";

   Delim1 : constant Path_Delimiters := Parse (Path1);
   Delim2 : constant Path_Delimiters := Parse (Path2);
   Delim3 : constant Path_Delimiters := Parse (Path3);
   Delim4 : constant Path_Delimiters := Parse (Path4);

--     procedure Pretty_Print (Path : New_Path);
--
--     ------------------
--     -- Pretty_Print --
--     ------------------
--
--     procedure Pretty_Print (Path : New_Path) is
--        Block : Integer := Path.Delim'First;
--     begin
--        Put_Line ("Path : " & Path.Str);
--        if Path.Delim'Length = 0 then
--           Put_Line ("No delimiters.");
--           return;
--        end if;
--
--        Put ("       ");
--        for Index in Path.Str'Range loop
--           if Index = Path.Delim (Block).From
--             or else
--               Index = Path.Delim (Block).To
--           then
--              Put ('|');
--           else
--              Put (' ');
--           end if;
--           if Index = Path.Delim (Block).To then
--              if Block = Path.Delim'Last then
--                 exit;
--              else
--                 Block := Block + 1;
--              end if;
--           end if;
--        end loop;
--        New_Line;
--
--        for D of Path.Delim loop
--           Put_Line ("Delim from:" & D.From'Img & " To:" & D.To'Img & " -> '" &
--                       Path.Str (D.From .. D.To) & "'");
--        end loop;
--     end Pretty_Print;

   New1 : constant New_Path := Parse (Path1);
begin

   pragma Assert (New1.Delimiters_Length = 4, "Wrong New1.Delimiter_Length");
   pragma Assert (Slice (New1, 1) = "1", "Wrong New1 root slice : " & Slice (New1, 1));
   pragma Assert (Slice (New1, 2) = "22", "Wrong New1 slice 2");
   pragma Assert (Slice (New1, 3) = "333", "Wrong New1 slice 3");
   pragma Assert (Slice (New1, 4) = "4444.txt", "Wrong New1 slice 4");

   pragma Assert (Delim1'Length = 4, "Wrong Delim1'Length :" & Delim1'Length'Img);
   pragma Assert (Delim2'Length = 0, "Wrong Delim2'Length :" & Delim2'Length'Img);
   pragma Assert (Delim3'Length = 2, "Wrong Delim3'Length :" & Delim3'Length'Img);
   pragma Assert (Delim4'Length = 2, "Wrong Delim4'Length :" & Delim4'Length'Img);

   pragma Assert (Delim1'First = 1, "Delim1'First is not 1");
   pragma Assert (Delim2'First = 1, "Delim2'First is not 1");
   pragma Assert (Delim3'First = 1, "Delim3'First is not 1");
   pragma Assert (Delim4'First = 1, "Delim4'First is not 1");

   pragma Assert (Root (Path1, Delim1) = "1", "Wrong path1 root slice");
   pragma Assert (Slice (Path1, Delim1, 2) = "22", "Wrong path1 slice 2");
   pragma Assert (Slice (Path1, Delim1, 3) = "333", "Wrong path1 slice 3");
   pragma Assert (Slice (Path1, Delim1, 4) = "4444.txt", "Wrong path1 slice 4");

   pragma Assert (Root (Path3, Delim3) = "a", "Wrong path3 root slice");
   pragma Assert (Slice (Path3, Delim3, 2) = "b.txt", "Wrong path3 slice 2");

   pragma Assert (Root (Path4, Delim4) = "x", "Wrong path4 root slice");
   pragma Assert (Slice (Path4, Delim4, 2) = "y.txt", "Wrong path4 slice 2");

   Ada.Text_IO.Put_Line ("TC_Path_Parse: PASS");
end TC_Path_Parse;
