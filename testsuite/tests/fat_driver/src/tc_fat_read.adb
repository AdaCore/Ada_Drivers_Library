with Ada.Text_IO; use Ada.Text_IO;

with Native.Filesystem;  use Native.Filesystem;
with HAL.Filesystem;     use HAL.Filesystem;
with Test_Directories;   use Test_Directories;
with File_Block_Drivers; use File_Block_Drivers;

with Filesystem.VFS;     use Filesystem.VFS;
with Filesystem.FAT;     use Filesystem.FAT;
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
      Handle : Any_Directory_Handle;
      Status : Status_Code;
      Node   : Any_Node_Handle;
   begin
      Put_Line ("Checking directory: '" & Dirname & "'");
      Status := Filesystem.VFS.Open (Dirname, Handle);

      if Status /= OK then
         Put_Line ("Cannot open directory: '" & Dirname & "'");
         Put_Line ("Status: " & Status'Img);
         return False;
      end if;

      loop
         if Handle.Read (Node) = OK and then Node /= null then
            if Node.Basename = "." or else Node.Basename = ".." then
               null; --  do nothing
            elsif Node.Is_Subdirectory then

               if not Check_Dir (Dirname & "/" & Node.Basename) then
                  return False;
               end if;
            elsif not Node.Is_Symlink then
               if not Check_File (Node.Basename, Dirname) then
                  return False;
               end if;
            end if;
         else
            exit;
         end if;
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
      Handle : Any_File_Handle;
      Status : Status_Code;
      Path   : constant String := Dirname & "/" & Basename;
   begin
      Status := Filesystem.VFS.Open (Path, Read_Mode, Handle);

      if Status /= OK then
         Put_Line ("Cannot open file: '" & Path & "'");
         Put_Line ("Status: " & Status'Img);
         return False;
      end if;

      declare
         Hash_Str : constant String := Compare_Files.Compute_Hash (Handle);
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
      Handle : Any_File_Handle;
      Status : Status_Code;
      Path   : constant String := "/disk_img/number_of_files_to_check";
      C      : Character;
      Amount : File_Size;
   begin
      Status := Filesystem.VFS.Open (Path, Read_Mode, Handle);

      if Status /= OK then
         Put_Line ("Cannot open file: '" & Path & "'");
         Put_Line ("Status: " & Status'Img);
         return False;
      end if;

      Amount := 1;
      Status := Handle.Read (C'Address, Amount);

      if Status /= OK then
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

   FS                 : aliased Native_FS_Driver;
   Disk_Img           : HAL.Filesystem.Any_File_Handle;
   Disk_Img_Path      : constant String := "fat.fs";

   Status : Status_Code;
begin

   if FS.Create (Root_Dir => Test_Dir) /= OK then
      raise Program_Error with "Cannot create native file system at '" &
        Test_Dir & "'";
   end if;

   if FS.Open (Disk_Img_Path, Read_Mode, Disk_Img) /= OK then
      Put_Line ("Cannot open disk image '" & Disk_Img_Path & "'");
      return;
   end if;

   declare
      Disk   : aliased File_Block_Driver (Disk_Img);
      FAT_FS : access FAT_Filesystem;
   begin
      FAT_FS := new FAT_Filesystem;
      Status := Open (Controller => Disk'Unchecked_Access,
                      LBA        => 0,
                      FS         => FAT_FS.all);

      if Status /= OK then
         Put_Line ("Cannot open FAT FS - Status:" & Status'Img);
         return;
      end if;

      Status := Filesystem.VFS.Mount_Volume (Mount_Point => "disk_img",
                                             FS          => FAT_FS);
      if Status /= OK then
         Put_Line ("Cannot mount volume - Status: " & Status'Img);
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

   end;
end TC_FAT_Read;
