with Ada.Text_IO; use Ada.Text_IO;

with Native.Filesystem;   use Native.Filesystem;
with Virtual_File_System; use Virtual_File_System;

with Helpers; use Helpers;

procedure TC_Nested_Mount is
   Strict_Mode : constant Boolean := False;
   --  Debug switch to enable strict mode. Currently, non-strict mode avoids
   --  calls that are more or less expected to fail.

   Root_VFS   : Virtual_File_System.VFS;
   Child_VFS  : Virtual_File_System.Any_VFS := new Virtual_File_System.VFS;
   Empty_FS   : Native_FS_Driver_Ref :=
     Create ("empty", Create_If_Missing => True);
   Subdirs_FS : Native_FS_Driver_Ref := Create ("subdirs");
begin
   Test (Root_VFS.Mount ("empty", Empty_FS.all'Access));
   Test (Root_VFS.Mount ("child_vfs", Child_VFS.all'Access));
   Test (Child_VFS.Mount ("subdirs", Subdirs_FS.all'Access));
   Dump (Root_VFS, "/");

   --  Unmounting is not implemented yet

   if Strict_Mode then
      Test (Root_VFS.Umount ("empty"));
      Test (Root_VFS.Umount ("child_vfs"));
      Test (Child_VFS.Umount ("subdirs"));
   end if;

   Destroy (Child_VFS);
   Destroy (Empty_FS);
   Destroy (Subdirs_FS);
   Put_Line ("Done.");
end TC_Nested_Mount;
