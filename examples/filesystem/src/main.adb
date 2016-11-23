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

with HAL;           use HAL;
with Virtual_File_System; use Virtual_File_System;
with HAL.Filesystem; use HAL.Filesystem;
with Semihosting;
with Semihosting.Filesystem; use Semihosting.Filesystem;
with File_Block_Drivers; use File_Block_Drivers;
with Partitions; use Partitions;

procedure Main is

   procedure List_Dir (FS : in out FS_Driver'Class;
                       Path : Pathname);
   --  List files in directory

   procedure List_Partitions (FS : in out FS_Driver'Class;
                              Path_To_Disk_Image : Pathname);
   --  List partition in a disk file

   --------------
   -- List_Dir --
   --------------

   procedure List_Dir (FS : in out FS_Driver'Class; Path : Pathname) is
      Status : Status_Kind;
      DH : Directory_Handle_Ref;
   begin
      Status := FS.Open_Directory (Path, DH);
      if Status /= Status_Ok then
         Semihosting.Log_Line ("Open Directory '" & Path & "' Error: " & Status'Img);
      else
         declare
            Ent : Directory_Entry;
            Index : Positive := 1;
         begin

            Semihosting.Log_Line ("Listing '" & Path & "' content:");
            loop
               Status := DH.Read_Entry (Index, Ent);
               if Status = Status_Ok then
                  Semihosting.Log_Line (" - '" & DH.Entry_Name (Index) & "'");
                  Semihosting.Log_Line ("   Kind: " & Ent.Entry_Type'Img);
               else
                  exit;
               end if;
               Index := Index + 1;
            end loop;
         end;
      end if;
   end List_Dir;

   ---------------------
   -- List_Partitions --
   ---------------------

   procedure List_Partitions (FS : in out FS_Driver'Class;
                              Path_To_Disk_Image : Pathname)
   is
      File : File_Handle_Ref;
   begin
      if FS.Open (Path_To_Disk_Image, Read_Only, File) /= Status_Ok then
         Semihosting.Log_Line ("Cannot open disk image '" &
                                 Path_To_Disk_Image & "'");
         return;
      end if;

      declare
         Disk    : aliased File_Block_Driver (File);
         Nbr     : Natural;
         P_Entry : Partition_Entry;
      begin
         Nbr := Number_Of_Partitions (Disk'Unchecked_Access);
         Semihosting.Log_Line ("Disk '" & Path_To_Disk_Image & "' has " &
                                 Nbr'Img & " parition(s)");

         for Id in 1 .. Nbr loop
            if Get_Partition_Entry (Disk'Unchecked_Access,
                                    Id,
                                    P_Entry) /= Status_Ok
            then
               Semihosting.Log_Line ("Cannot read partition :" & Id'Img);
            else
               Semihosting.Log_Line (" - partition :" & Id'Img);
               Semihosting.Log_Line ("      Status:" & P_Entry.Status'Img);
               Semihosting.Log_Line ("      Kind: " & P_Entry.Kind'Img);
               Semihosting.Log_Line ("      LBA: " & P_Entry.First_Sector_LBA'Img);
               Semihosting.Log_Line ("      Number of sectors: " & P_Entry.Number_Of_Sectors'Img);
            end if;
         end loop;
      end;
   end List_Partitions;

   My_VFS : VFS;
   My_VFS2 : aliased VFS;
   My_VFS3 : aliased VFS;
   My_SHFS : aliased SHFS;
   Status : Status_Kind;
   FH : File_Handle_Ref;
   Data : Byte_Array (1 .. 10);

begin

   --  Mount My_VFS2 in My_VFS
   Status := My_VFS.Mount (Path       => "vfs2",
                           Filesystem => My_VFS2'Unchecked_Access);
   if Status /= Status_Ok then
      Semihosting.Log_Line ("Mount Error: " & Status'Img);
   end if;

      --  Mount My_VFS3 in My_VFS2
   Status := My_VFS2.Mount (Path       => "vfs3",
                            Filesystem => My_VFS3'Unchecked_Access);
   if Status /= Status_Ok then
      Semihosting.Log_Line ("Mount Error: " & Status'Img);
   end if;

   --  Mount semi-hosting filesystem in My_VFS
   Status := My_VFS.Mount (Path       => "host",
                            Filesystem => My_SHFS'Unchecked_Access);
   if Status /= Status_Ok then
      Semihosting.Log_Line ("Mount Error: " & Status'Img);
   end if;

   --  List all partitions of a disk image on the host
   List_Partitions (My_VFS, "/host/tmp/disk_8_partitions.img");

   --  Try to unlink a file that doesn't exist in My_VFS2
   Status := My_VFS.Unlink ("/vfs2/no_file");
   if Status /= Status_Ok then
      Semihosting.Log_Line ("Unlink Error: " & Status'Img);
   end if;

   --  Try to unlink a file that doesn't exist in My_VFS3
   Status := My_VFS.Unlink ("/vfs2/vfs3/no_file");
   if Status /= Status_Ok then
      Semihosting.Log_Line ("Unlink Error: " & Status'Img);
   end if;

   --  Open a file on the host
   Status := My_VFS.Open ("/host/tmp/test.shfs", Read_Only, FH);
   if Status /= Status_Ok then
      Semihosting.Log_Line ("Open Error: " & Status'Img);
   end if;

   --  Read the first 10 characters
   Status := FH.Read (Data);
   if Status /= Status_Ok then
      Semihosting.Log_Line ("Read Error: " & Status'Img);
   end if;

   for C of Data loop
      Semihosting.Log (Character'Val (Integer (C)));
   end loop;
   Semihosting.Log_New_Line;

   --  Move file cursor
   Status := FH.Seek (10);
   if Status /= Status_Ok then
      Semihosting.Log_Line ("Seek Error: " & Status'Img);
   end if;

   --  Read 10 characters again
   Status := FH.Read (Data);
   if Status /= Status_Ok then
      Semihosting.Log_Line ("Read Error: " & Status'Img);
   end if;

   for C of Data loop
      Semihosting.Log (Character'Val (Integer (C)));
   end loop;
   Semihosting.Log_New_Line;

   Status := FH.Close;
   if Status /= Status_Ok then
      Semihosting.Log_Line ("Close Error: " & Status'Img);
   end if;

   --  Test directory listing
   List_Dir (My_VFS, "/");
   List_Dir (My_VFS, "/vfs2");
   List_Dir (My_VFS, "/vfs2/");
end Main;
