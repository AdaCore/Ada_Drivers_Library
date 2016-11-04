with Ada.Text_IO; use Ada.Text_IO;

with Native.Filesystem;   use Native.Filesystem;
with Virtual_File_System; use Virtual_File_System;

with Helpers; use Helpers;

procedure TC_Simple_Mount is
   Strict_Mode : constant Boolean := False;
   --  Debug switch to enable strict mode. Currently, non-strict mode avoids
   --  calls that are more or less expected to fail.

   VFS : Virtual_File_System.VFS;
   HFS : Native_FS_Driver_Ref := Create ("one_file");
begin
   Put_Line ("Dumping the ""one_file"" mounted as /one_file:");
   Test (VFS.Mount ("one_file", HFS.all'Access));
   Dump (VFS, "/");

   --  Unmounting is not implemented yet

   if Strict_Mode then
      Test (VFS.Umount ("one_file"));
   end if;

   Destroy (HFS);
   Put_Line ("Done.");
end TC_Simple_Mount;
