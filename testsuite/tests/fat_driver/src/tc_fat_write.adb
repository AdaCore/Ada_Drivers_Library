with Ada.Text_IO;        use Ada.Text_IO;

with Native.Filesystem;  use Native.Filesystem;
with HAL.Filesystem;     use HAL.Filesystem;
with Test_Directories;   use Test_Directories;
with File_Block_Drivers; use File_Block_Drivers;

with Filesystem.VFS;     use Filesystem.VFS;
with Filesystem.FAT;     use Filesystem.FAT;

with GNAT.MD5;           use GNAT.MD5;
with Ada.Streams;
with Compare_Files;
with Copy_Files;

procedure TC_FAT_Write is

   package Hash renames GNAT.MD5;

   Test_File_Size : constant := 2000;

   function Write_File (Filename : String) return String;
   function Check_File (Filename : String; Md5 : String) return Boolean;
   function Delete_Tree (Filename : String) return Boolean;

   ----------------
   -- Write_File --
   ----------------

   function Write_File (Filename : String) return String is
      Handle : Any_File_Handle;
      Status : Status_Code;

      Context : aliased GNAT.MD5.Context := GNAT.MD5.Initial_Context;

      Buffer : Ada.Streams.Stream_Element_Array (1 .. Test_File_Size);
      Last   : Ada.Streams.Stream_Element_Offset;
      Size   : File_Size;
      use type Ada.Streams.Stream_Element_Offset;

   begin
      Status := Filesystem.VFS.Open (Filename, Write_Mode, Handle);

      if Status /= OK then
         Put_Line ("Cannot open file: '" & Filename & "'");
         Put_Line ("Status: " & Status'Img);
         return "";
      end if;

      Size := Buffer'Length;
      Buffer := (others => 42);
      Last := Ada.Streams.Stream_Element_Offset (Size);
      Hash.Update (Context, Buffer (1 .. Last));
      Status := Handle.Write (Addr   => Buffer'Address,
                              Length => Size);

      if Status /= OK then
         Put_Line ("Cannot write file: '" & Filename & "'");
         Put_Line ("Status: " & Status'Img);
         return "";
      end if;

      Handle.Close;
      return Hash.Digest (Context);
   end Write_File;

   ----------------
   -- Check_File --
   ----------------

   function Check_File (Filename : String;
                        Md5      : String)
                        return Boolean
   is
      Handle : Any_File_Handle;
      Status : Status_Code;
   begin
      Status := Filesystem.VFS.Open (Filename, Read_Mode, Handle);

      if Status /= OK then
         Put_Line ("Cannot open file: '" & Filename & "'");
         Put_Line ("Status: " & Status'Img);
         return False;
      end if;

      if Handle.Size /= Test_File_Size then
         Put_Line ("Error: wrong file size: " & Handle.Size'Img &
                     " (expected " & Test_File_Size'Img & ")");
      end if;

      declare
         Hash_Str : constant String := Compare_Files.Compute_Hash (Handle);
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
      Handle : Any_Directory_Handle;
      Status : Status_Code;
      Node   : Any_Node_Handle;
   begin
      Status := Filesystem.VFS.Open (Filename, Handle);

      if Status /= OK then
         Put_Line ("Cannot open directory: '" & Filename & "'");
         Put_Line ("Status: " & Status'Img);
         return False;
      end if;

      loop
         if Handle.Read (Node) = OK and then Node /= null then
            declare
               Sub : constant String := Filename & "/" & Node.Basename;
            begin
               if Node.Basename = "." or else Node.Basename = ".." then
                  null; --  do nothing
               elsif Node.Is_Subdirectory then
                  if not Delete_Tree (Sub) then
                     return False;
                  end if;
               elsif not Node.Is_Symlink then
                  Status := Filesystem.VFS.Unlink (Sub);
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
      end loop;

      Status := Filesystem.VFS.Remove_Directory (Filename);
      if  Status /= OK then
         Put_Line ("Cannot delete dir: '" & Filename & "' :" & Status'Img);
         return False;
      else
         return True;
      end if;
   end Delete_Tree;

   FS            : aliased Native_FS_Driver;
   Disk_Img      : HAL.Filesystem.Any_File_Handle;
   Disk_Img_Path      : constant String := "fat.fs";
   Copy_Disk_Img_Path : constant String := "obj/fat.fs.copy";

   Status : Status_Code;
begin

   --  Make a copy of the disk image
   if not Copy_Files.Copy (Test_Dir & "/" & Disk_Img_Path,
                           Test_Dir & "/" & Copy_Disk_Img_Path)
   then
      raise Program_Error with "Cannot copy disk image";
   end if;

   if FS.Create (Root_Dir => Test_Dir) /= OK then
      raise Program_Error with "Cannot create native file system at '" &
        Test_Dir & "'";
   end if;

   if FS.Open (Copy_Disk_Img_Path, Read_Write_Mode, Disk_Img) /= OK then
      Put_Line ("Cannot open disk image '" & Copy_Disk_Img_Path & "'");
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

      --  Make some room for the file that we will create
      if not Delete_Tree ("/disk_img/read_test") then
         Put_Line ("Cannot delete directory");
      end if;

      if Check_File (Filename => "/disk_img/write_test",
                     Md5      => Write_File ("/disk_img/write_test"))
      then
         Put_Line ("PASS");
      else
         Put_Line ("FAIL");
      end if;

   end;
end TC_FAT_Write;
