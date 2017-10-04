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

with HAL;                    use HAL;
with Semihosting;
with Semihosting.Filesystem; use Semihosting.Filesystem;
with File_Block_Drivers;     use File_Block_Drivers;
with Partitions;
with File_IO;                use File_IO;

procedure Main is

   use type Partitions.Status_Code;

   procedure List_Dir (Path : String);
   --  List files in directory

   procedure List_Partitions (Path_To_Disk_Image : String);
   --  List partition in a disk file

   --------------
   -- List_Dir --
   --------------

   procedure List_Dir (Path : String) is
      Status : Status_Code;
      DD     : Directory_Descriptor;
   begin
      Status := Open (DD, Path);
      if Status /= OK then
         Semihosting.Log_Line ("Open Directory '" & Path & "' Error: " & Status'Img);
      else
         Semihosting.Log_Line ("Listing '" & Path & "' content:");
         loop
            declare
               Ent : constant Directory_Entry := Read (DD);
            begin
               if Ent /= Invalid_Dir_Entry then
                  Semihosting.Log_Line (" - '" & Ent.Name & "'");
                  Semihosting.Log_Line ("   Kind: " & Ent.Subdirectory'Img);
               else
                  exit;
               end if;
            end;
         end loop;
      end if;
   end List_Dir;

   ---------------------
   -- List_Partitions --
   ---------------------

   procedure List_Partitions (Path_To_Disk_Image : String)
   is
      Nbr     : Natural;
      P_Entry : Partitions.Partition_Entry;
      Disk    : aliased File_Block_Driver;
      Status  : Status_Code;
   begin
      Status := Disk.Open (Path_To_Disk_Image, Read_Only);
      if Status /= OK then
         Semihosting.Log_Line ("Cannot open disk image '" &
                                 Path_To_Disk_Image & "' : " &
                                 Status'Img);
         return;
      end if;

      Nbr := Partitions.Number_Of_Partitions (Disk'Unchecked_Access);
      Semihosting.Log_Line ("Disk '" & Path_To_Disk_Image & "' has " &
                              Nbr'Img & " parition(s)");

      for Id in 1 .. Nbr loop
         if Partitions.Get_Partition_Entry
           (Disk'Unchecked_Access,
            Id,
            P_Entry) /= Partitions.Status_Ok
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
   end List_Partitions;

   My_SHFS : aliased SHFS;
   Status  : Status_Code;
   FD      : File_Descriptor;
   Data    : UInt8_Array (1 .. 10);
   Amount  : File_Size;
begin

   --  Mount semi-hosting filesystem in My_VFS
   Status := Mount_Volume ("host", My_SHFS'Unchecked_Access);
   if Status /= OK then
      Semihosting.Log_Line ("Mount Error: " & Status'Img);
   end if;


   --  Open a file on the host
   Status := Open (FD, "/host/tmp/test.shfs", Read_Only);
   if Status /= OK then
      Semihosting.Log_Line ("Open Error: " & Status'Img);
      return;
   end if;

   --  Read the first 10 characters
   if Read (FD, Data'Address, Data'Length) /= Data'Length then
      Semihosting.Log_Line ("Read Error: " & Status'Img);
      return;
   end if;

   for C of Data loop
      Semihosting.Log (Character'Val (Integer (C)));
   end loop;
   Semihosting.Log_New_Line;

   --  Move file cursor
   Amount := 10;
   Status := Seek (FD, Forward, Amount);
   if Status /= OK then
      Semihosting.Log_Line ("Seek Error: " & Status'Img);
      return;
   end if;

   --  Read the 10 characters more
   if Read (FD, Data'Address, Data'Length) /= Data'Length then
      Semihosting.Log_Line ("Read Error: " & Status'Img);
      return;
   end if;

   for C of Data loop
      Semihosting.Log (Character'Val (Integer (C)));
   end loop;

   Semihosting.Log_New_Line;

   Close (FD);

   --  List all partitions of a disk image on the host
   List_Partitions ("/host/tmp/disk_8_partitions.img");

   --  Test directory listing
   List_Dir ("/");
end Main;
