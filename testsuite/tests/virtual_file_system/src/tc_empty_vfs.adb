with Ada.Text_IO; use Ada.Text_IO;

with Virtual_File_System; use Virtual_File_System;

with Helpers; use Helpers;

procedure TC_Empty_VFS is
   VFS    : Virtual_File_System.VFS;
begin
   Put_Line ("Dumping the root directory of an empty VFS:");
   Dump (VFS, "/");
   Put_Line ("Done.");
end TC_Empty_VFS;
