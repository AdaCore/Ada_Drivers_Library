with Ada.Text_IO; use Ada.Text_IO;

with Filesystem.Native;  use Filesystem.Native;
with Test_Directories;   use Test_Directories;
with File_Block_Drivers; use File_Block_Drivers;
with File_IO;            use File_IO;
with Filesystem.FAT;     use Filesystem.FAT;
with HAL.Filesystem;     use HAL.Filesystem;
with Compare_Files;

procedure TC_FAT_Read is

   function Check_Dir (Dirname : String) return Boolean;
   function Check_File (Basename : String;
                        Dirname  : String)
                        return Boolean;
   function Check_Expected_Number return Boolean;

   Number_Of_Files_Checked : Natural := 0;

   ---------------
   -- Check_Dir --
   ---------------

   function Check_Dir (Dirname : String) return Boolean is
      DD     : Directory_Descriptor;
      Status : File_IO.Status_Code;
   begin
      Put_Line ("Checking directory: '" & Dirname & "'");
      Status := Open (DD, Dirname);

      if Status /= OK then
         Put_Line ("Cannot open directory: '" & Dirname & "'");
         Put_Line ("Status: " & Status'Img);
         return False;
      end if;

      loop
         declare
            Ent : constant Directory_Entry := Read (DD);
         begin
            if Ent /= Invalid_Dir_Entry then
               if Ent.Name = "." or else Ent.Name = ".." then
                  null; --  do nothing
               elsif Ent.Subdirectory then

                  if not Check_Dir (Dirname & "/" & Ent.Name) then
                     return False;
                  end if;
               elsif not Ent.Symlink then
                  if not Check_File (Ent.Name, Dirname) then
                     return False;
                  end if;
               end if;
            else
               exit;
            end if;
         end;
      end loop;

      return True;
   end Check_Dir;

   ----------------
   -- Check_File --
   ----------------

   function Check_File (Basename : String;
                        Dirname  : String)
                        return Boolean
   is
      FD     : File_Descriptor;
      Status : File_IO.Status_Code;
      Path   : constant String := Dirname & "/" & Basename;
   begin
      Status := Open (FD, Path, Read_Only);

      if Status /= OK then
         Put_Line ("Cannot open file: '" & Path & "'");
         Put_Line ("Status: " & Status'Img);
         return False;
      end if;

      declare
         Hash_Str : constant String := Compare_Files.Compute_Hash (FD);
      begin
         if Hash_Str /= Basename then
            Put_Line ("Error: Hash is different than filename");
            return False;
         else
            Number_Of_Files_Checked := Number_Of_Files_Checked + 1;
            return True;
         end if;
      end;
   end Check_File;

   ---------------------------
   -- Check_Expected_Number --
   ---------------------------

   function Check_Expected_Number return Boolean is
      FD     : File_Descriptor;
      Status : File_IO.Status_Code;
      Path   : constant String := "/disk_img/number_of_files_to_check";
      C      : Character;
      Amount : File_IO.File_Size;
   begin
      Status := Open (FD, Path, Read_Only);

      if Status /= OK then
         Put_Line ("Cannot open file: '" & Path & "'");
         Put_Line ("Status: " & Status'Img);
         return False;
      end if;

      Amount := 1;
      if Read (FD, C'Address, Amount) /= Amount then
         Put_Line ("Cannot read file: '" & Path & "'");
         Put_Line ("Status: " & Status'Img);
         return False;
      end if;

      if C in '0' .. '9'
        and then
          Number_Of_Files_Checked = (Character'Pos (C) - Character'Pos ('0'))
      then
         return True;
      else
         Put_Line ("Incorrect number of files");
         return False;
      end if;
   end Check_Expected_Number;

   Disk_Img_Path : constant String := "/test_dir/fat.fs";
   Disk          : aliased File_Block_Driver;
   FAT_FS        : access FAT_Filesystem;
   FIO_Status    : File_IO.Status_Code;
   HALFS_Status  : HAL.Filesystem.Status_Code;
begin

   Test_Directories.Mount_Test_Directory ("test_dir");

   FIO_Status := Disk.Open (Disk_Img_Path, Read_Only);
   if FIO_Status /= OK then
      Put_Line ("Cannot open disk image '" & Disk_Img_Path & "': " &
                  FIO_Status'Img);
      return;
   end if;

   FAT_FS := new FAT_Filesystem;
   HALFS_Status := Open (Controller => Disk'Unchecked_Access,
                         LBA        => 0,
                         FS         => FAT_FS.all);

   if HALFS_Status /= OK then
      Put_Line ("Cannot open FAT FS - Status:" & HALFS_Status'Img);
      return;
   end if;

   FIO_Status := File_IO.Mount_Volume (Mount_Point => "disk_img",
                                       FS          => FAT_FS);
   if FIO_Status /= OK then
      Put_Line ("Cannot mount volume - Status: " & FIO_Status'Img);
      return;
   end if;

   if Check_Dir ("/disk_img/read_test")
     and then
       Check_Expected_Number
   then
      Put_Line ("PASS");
   else
      Put_Line ("FAIL");
   end if;

end TC_FAT_Read;
