with Ada.Text_IO; use Ada.Text_IO;

with Native.Filesystem;   use Native.Filesystem;
with Filesystem.VFS;

with Helpers; use Helpers;

procedure TC_Simple_Mount is
   HFS : constant Native_FS_Driver_Access := Create ("one_file");
begin
   Put_Line ("Dumping the ""one_file"" mounted as /one_file:");
   Test (Filesystem.VFS.Mount_Volume ("one_file", HFS.all'Access));
   Dump ("/");

   Test (Filesystem.VFS.Unmount ("one_file"));

   Put_Line ("Done.");
end TC_Simple_Mount;
