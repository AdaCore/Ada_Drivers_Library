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

package body Command_Line.Filesystem.List_Directory is

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (This     : in out Ls_Cmd;
      Args     : in out Arguments'Class;
      Put      : Put_Procedure;
      Put_Line : Put_Line_Procedure)
   is
      pragma Unreferenced (This);

      Recursive : Boolean := False;
      Show_All  : Boolean := False;

      procedure List (Path : String);

      ----------
      -- List --
      ----------

      procedure List (Path : String) is
         DD     : Directory_Descriptor;
         Status : Status_Code;
         First  : Boolean := True;
      begin
         Status := Open (DD, Path);
         if Status /= OK then
            Put ("Cannot open directory '" & Path & "': " & Status'Img);
            return;
         end if;

         if Recursive then
            Put_Line (Path & ":");
         end if;

         loop
            declare
               Ent : constant Directory_Entry := Read (DD);
            begin
               exit when Ent = Invalid_Dir_Entry;

               if not Ent.Hidden or else Show_All then
                  if First then
                     Put (Ent.Name);
                     First := False;
                  else
                     Put (" " & Ent.Name);
                  end if;
               end if;
            end;
         end loop;

         Put_Line ("");

         if Recursive then
            Reset (DD);

            loop
               declare
                  Ent : constant Directory_Entry := Read (DD);
               begin
                  exit when Ent = Invalid_Dir_Entry;

                  if Ent.Subdirectory then
                     List (Path & "/" & Ent.Name);
                  end if;
               end;
            end loop;
         end if;

         Close (DD);
      end List;

   begin
      loop
         declare
            Arg : constant String := Args.Next;
         begin
            if Arg'Length = 0 then
               return;
            elsif Arg = "-r" or else Arg = "-R" then
               Recursive := True;
            elsif Arg = "-a" then
               Show_All := True;
            else
               List (Arg);
            end if;
         end;
      end loop;
   end Execute;

   ----------
   -- Help --
   ----------

   overriding
   procedure Help (This     : in out Ls_Cmd;
                   Put      : Put_Procedure;
                   Put_Line : Put_Line_Procedure)
   is
      pragma Unreferenced (Put, This);
   begin
      Put_Line ("Usage: ls [-a] [-r|-R] [FILE]...");
      Put_Line ("List information about files and directories.");
      Put_Line (" -a     : do not ignore hidden files or directories");
      Put_Line (" -r, -R : list subdirectories recursively");
   end Help;

end Command_Line.Filesystem.List_Directory;
