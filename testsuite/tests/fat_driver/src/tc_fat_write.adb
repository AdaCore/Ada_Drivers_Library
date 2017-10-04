with Ada.Text_IO;        use Ada.Text_IO;

with Filesystem.Native;  use Filesystem.Native;
with Test_Directories;   use Test_Directories;
with File_Block_Drivers; use File_Block_Drivers;
with File_IO;            use File_IO;
with Filesystem.FAT;     use Filesystem.FAT;
with HAL.Filesystem;

with GNAT.MD5;           use GNAT.MD5;
with Ada.Streams;
with Compare_Files;

procedure TC_FAT_Write is

   use type HAL.Filesystem.Status_Code;

   package Hash renames GNAT.MD5;

   Test_File_Size : constant := 2000;

   function Write_File (Filename : String) return String;
   function Check_File (Filename : String; Md5 : String) return Boolean;
   function Delete_Tree (Filename : String) return Boolean;
   function Check_Read_Test_Dir return Boolean;

   ----------------
   -- Write_File --
   ----------------

   function Write_File (Filename : String) return String is
      FD     : File_Descriptor;
      Status : Status_Code;

      Context : aliased GNAT.MD5.Context := GNAT.MD5.Initial_Context;

      Buffer : Ada.Streams.Stream_Element_Array (1 .. Test_File_Size);
      Last   : Ada.Streams.Stream_Element_Offset;
      Size   : File_Size;
      use type Ada.Streams.Stream_Element_Offset;

   begin
      Status := Open (FD, Filename, Write_Only);

      if Status /= OK then
         Put_Line ("Cannot open file: '" & Filename & "'");
         Put_Line ("Status: " & Status'Img);
         return "";
      end if;

      Size := Buffer'Length;
      Buffer := (others => 42);
      Last := Ada.Streams.Stream_Element_Offset (Size);
      Hash.Update (Context, Buffer (1 .. Last));

      if Write (FD, Buffer'Address, Size) /= Size then
         Put_Line ("Cannot write file: '" & Filename & "'");
         Put_Line ("Status: " & Status'Img);
         return "";
      end if;

      Close (FD);
      return Hash.Digest (Context);
   end Write_File;

   ----------------
   -- Check_File --
   ----------------

   function Check_File (Filename : String;
                        Md5      : String)
                        return Boolean
   is
      FD     : File_Descriptor;
      Status : Status_Code;
   begin
      Status := Open (FD, Filename, Read_Only);

      if Status /= OK then
         Put_Line ("Cannot open file: '" & Filename & "'");
         Put_Line ("Status: " & Status'Img);
         return False;
      end if;

      if Size (FD) /= Test_File_Size then
         Put_Line ("Error: wrong file size: " & Size (FD)'Img &
                     " (expected " & Test_File_Size'Img & ")");
         return False;
      end if;

      declare
         Hash_Str : constant String := Compare_Files.Compute_Hash (FD);
      begin
         if Hash_Str /= Md5 then
            Put_Line ("Error: Hash is different than filename");
            return False;
         else
            return True;
         end if;
      end;
   end Check_File;

   -----------------
   -- Delete_Tree --
   -----------------

   function Delete_Tree (Filename : String) return Boolean is
      Dir    : Directory_Descriptor;
      Status : Status_Code;
   begin
      Status := Open (Dir, Filename);

      if Status /= OK then
         Put_Line ("Cannot open directory: '" & Filename & "'");
         Put_Line ("Status: " & Status'Img);
         return False;
      end if;

      loop
         declare
            Ent : constant Directory_Entry := Read (Dir);
         begin

            if Ent /= Invalid_Dir_Entry then
               declare
                  Sub : constant String := Filename & "/" & Ent.Name;
               begin
                  if Ent.Name = "." or else Ent.Name = ".." then
                     null; --  do nothing
                  elsif Ent.Subdirectory then
                     if not Delete_Tree (Sub) then
                        return False;
                     end if;
                  elsif not Ent.Symlink then
                     Status := Unlink (Sub);
                     if Status /= OK then
                        Put_Line ("Cannot delete file: '" & Sub & "' :" &
                                    Status'Img);
                        return False;
                     end if;
                  end if;
               end;
            else
               exit;
            end if;
         end;
      end loop;

      Status := Remove_Directory (Filename);
      if  Status /= OK then
         Put_Line ("Cannot delete dir: '" & Filename & "' :" & Status'Img);
         return False;
      else
         return True;
      end if;
   end Delete_Tree;

   -------------------------
   -- Check_Read_Test_Dir --
   -------------------------

   function Check_Read_Test_Dir return Boolean is
      Dir      : Directory_Descriptor;
      Status   : Status_Code;
      Filename : constant String := "/disk_img/read_test/";
   begin
      Status := Open (Dir, Filename);

      if Status /= OK then
         Put_Line ("Cannot open directory: '" & Filename & "'");
         Put_Line ("Status: " & Status'Img);
         return False;
      else
         Close (Dir);
         return True;
      end if;
   end Check_Read_Test_Dir;


   Disk_Img_Path      : constant String := "/" & Test_Dir_Mount_Name & "/fat.fs";
   Copy_Disk_Img_Path : constant String := "/" & Test_Dir_Mount_Name & "/obj/fat.fs.copy";
   Disk               : aliased File_Block_Driver;
   FAT_FS             : access FAT_Filesystem;

   Status       : Status_Code;
   HALFS_Status : HAL.Filesystem.Status_Code;

begin

   Test_Directories.Mount_Test_Directory;

   --  Make a copy of the disk image
   if Copy_File (Disk_Img_Path, Copy_Disk_Img_Path) /= OK then
      raise Program_Error with "Cannot copy disk image";
   end if;

   Status := Disk.Open (Copy_Disk_Img_Path, Read_Write);
   if Status /= OK then
      Put_Line ("Cannot open disk image '" & Copy_Disk_Img_Path & "': " &
                  Status'Img);
      return;
   end if;

   declare
   begin
      FAT_FS := new FAT_Filesystem;
      HALFS_Status := Open (Controller => Disk'Unchecked_Access,
                            LBA        => 0,
                            FS         => FAT_FS.all);

      if HALFS_Status /= HAL.Filesystem.OK then
         Put_Line ("Cannot open FAT FS - Status:" & HALFS_Status'Img);
         return;
      end if;

      Status := Mount_Volume (Mount_Point => "disk_img",
                              FS          => FAT_FS);
      if Status /= OK then
         Put_Line ("Cannot mount volume - Status: " & Status'Img);
         return;
      end if;

      --  Check if the "read_test" directory is here
      if Check_Read_Test_Dir then
         Put_Line ("PASS");
      else
         Put_Line ("FAIL");
      end if;


      --  Make some room for the file that we will create
      if not Delete_Tree ("/disk_img/read_test/lvl1_b") then
         Put_Line ("Cannot delete directory lvl1_b");
      end if;
      if not Delete_Tree ("/disk_img/read_test/lvl1_c") then
         Put_Line ("Cannot delete directory lvl1_c");
      end if;

      if Check_File (Filename => "/disk_img/write_test",
                     Md5      => Write_File ("/disk_img/write_test"))
      then
         Put_Line ("PASS");
      else
         Put_Line ("FAIL");
      end if;

      --  Check if the "read_test" directory is still here
      if Check_Read_Test_Dir then
         Put_Line ("PASS");
      else
         Put_Line ("FAIL");
      end if;

   end;
end TC_FAT_Write;
