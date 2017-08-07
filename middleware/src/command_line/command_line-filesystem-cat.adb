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

with File_IO; use File_IO;

package body Command_Line.Filesystem.Cat is

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (This     : in out Cat_Cmd;
      Args     : in out Arguments'Class;
      Put      : Put_Procedure;
      Put_Line : Put_Line_Procedure)
   is
      pragma Unreferenced (This);
      procedure Display_File (Path : String);

      ------------------
      -- Display_File --
      ------------------

      procedure Display_File (Path : String) is
         FD     : File_Descriptor;
         Status : Status_Code;
         Data   : String (1 .. 512);
         Amount : File_Size;
      begin
         Status := Open (FD, Path, Read_Only);
         if Status /= OK then
            Put_Line ("Cannot open file '" & Path & "': " & Status'Img);
            return;
         end if;

         loop
            Amount := Read (FD, Data'Address, Data'Length);
            exit when Amount = 0;
            Put (Data (Data'First .. Data'First + Natural (Amount) - 1));
         end loop;
         Close (FD);
      end Display_File;

   begin
      loop
         declare
            Arg : constant String := Args.Next;
         begin
            if Arg'Length = 0 then
               return;
            end if;
            Display_File (Arg);
         end;
      end loop;
   end Execute;

   ----------
   -- Help --
   ----------

   overriding
   procedure Help (This     : in out Cat_Cmd;
                   Put      : Put_Procedure;
                   Put_Line : Put_Line_Procedure)
   is
      pragma Unreferenced (Put, This);
   begin
      Put_Line ("Usage: cat [FILE]...");
      Put_Line ("Print file(s).");
   end Help;

end Command_Line.Filesystem.Cat;
