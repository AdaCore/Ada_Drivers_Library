with Ada.Text_IO; use Ada.Text_IO;

with Native.Filesystem;  use Native.Filesystem;
with HAL.Filesystem;     use HAL.Filesystem;
with Test_Directories;   use Test_Directories;
with File_Block_Drivers; use File_Block_Drivers;

with Filesystem.VFS;     use Filesystem.VFS;
with Filesystem;         use Filesystem;
with Filesystem.FAT;     use Filesystem.FAT;

with GNAT.MD5;           use GNAT.MD5;
with Ada.Streams;

procedure TC_FAT is

   package Hash renames GNAT.MD5;

   function Check_Dir (Dirname : String) return Boolean;
   function Check_File (Basename : String;
                        Dirname  : String)
                        return Boolean;

   function Compute_Hash (Handle : Filesystem.Any_File_Handle)
                          return Message_Digest;

   ------------------
   -- Compute_Hash --
   ------------------

   function Compute_Hash (Handle : Filesystem.Any_File_Handle)
                          return Message_Digest
   is
      Context : aliased GNAT.MD5.Context := GNAT.MD5.Initial_Context;

      Buffer : Ada.Streams.Stream_Element_Array (1 .. 512);
      Last   : Ada.Streams.Stream_Element_Offset;
      Size   : Filesystem.File_Size;
      Status : Status_Code;
      pragma Unreferenced (Status);
      use type Ada.Streams.Stream_Element_Offset;
   begin
      loop
         Size := Buffer'Length;
         Status := Handle.Read (Addr   => Buffer'Address,
                                Length => Size);
         Last := Ada.Streams.Stream_Element_Offset (Size);
         Hash.Update (Context, Buffer (1 .. Last));
         exit when Last < Buffer'Last;
      end loop;
      return Hash.Digest (Context);
   end Compute_Hash;

   ---------------
   -- Check_Dir --
   ---------------

   function Check_Dir (Dirname : String) return Boolean is
      Handle : Filesystem.Any_Directory_Handle;
      Status : Status_Code;
      Node   : Filesystem.Any_Node_Handle;
   begin
      Put_Line ("Checking directory: '" & Dirname & "'");
      Handle := Filesystem.VFS.Open (Dirname, Status);

      if Status /= OK then
         Put_Line ("Cannot open directory: '" & Dirname & "'");
         Put_Line ("Status: " & Status'Img);
         return False;
      end if;

      loop
         Node := Handle.Read (Status);
         if Status = OK and then Node /= null then
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
      Handle : Filesystem.Any_File_Handle;
      Status : Status_Code;
      Path   : constant String := Dirname & "/" & Basename;
   begin
      Put_Line ("Checking file: '" & Path & "'");

      Handle := Filesystem.VFS.Open (Path, Read_Mode, Status);

      if Status /= OK then
         Put_Line ("Cannot open file: '" & Path & "'");
         Put_Line ("Status: " & Status'Img);
         return False;
      end if;
      Put_Line ("File size :" & Handle.Size'Img);
      declare
         Hash_Str : constant String := Compute_Hash (Handle);
      begin
         Put_Line ("Compute Hash:" & Hash_Str);

         if Hash_Str /= Basename then
            Put_Line ("Error: Hash is different than filename");
            return False;
         else
            return True;
         end if;
      end;
   end Check_File;

   FS            : aliased Native_FS_Driver;
   Disk_Img      : HAL.Filesystem.Any_File_Handle;
   Disk_Img_Path : constant String := "fat.fs";

   Status : Status_Code;
begin
   if FS.Create (Root_Dir => Test_Dir) /= Status_Ok then
      raise Program_Error with "Cannot create native file system at '" &
        Test_Dir & "'";
   end if;

   if FS.Open (Disk_Img_Path, Read_Only, Disk_Img) /= Status_Ok then
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
                      FS         => FAT_FS);

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

      if Check_Dir ("/disk_img") then
         Put_Line ("PASS");
      else
         Put_Line ("FAIL");
      end if;

   end;
end TC_FAT;
