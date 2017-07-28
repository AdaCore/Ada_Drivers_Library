with Ada.Text_IO; use Ada.Text_IO;

with Helpers; use Helpers;

procedure TC_Empty_VFS is
begin
   Put_Line ("Dumping the root directory of an empty VFS:");
   Dump ("/");
   Put_Line ("Done.");
end TC_Empty_VFS;
