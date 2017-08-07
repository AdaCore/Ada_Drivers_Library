with Ada.Text_IO; use Ada.Text_IO;

with Command_Line;
with Command_Line.Filesystem;
with Test_Directories;

procedure TC_CMD_Filesystem is

   Cmd_1 : aliased constant String := "help";
   Cmd_2 : aliased constant String := "help ls";
   Cmd_3 : aliased constant String := "ls /test_dir/test_material";
   Cmd_4 : aliased constant String := "ls -r /test_dir/test_material";
   Cmd_5 : aliased constant String := "ls -r -a /test_dir/test_material";
   Cmd_6 : aliased constant String := "cat /test_dir/test_material/test.txt";
begin

   Test_Directories.Mount_Test_Directory;

   Command_Line.Filesystem.Register_All;

   Ada.Text_IO.Put_Line ("$ " & Cmd_1);
   Command_Line.Run (Cmd_1, Put'Access, Put_Line'Access);
   Ada.Text_IO.Put_Line ("$ " & Cmd_2);
   Command_Line.Run (Cmd_2, Put'Access, Put_Line'Access);
   Ada.Text_IO.Put_Line ("$ " & Cmd_3);
   Command_Line.Run (Cmd_3, Put'Access, Put_Line'Access);
   Ada.Text_IO.Put_Line ("$ " & Cmd_4);
   Command_Line.Run (Cmd_4, Put'Access, Put_Line'Access);
   Ada.Text_IO.Put_Line ("$ " & Cmd_5);
   Command_Line.Run (Cmd_5, Put'Access, Put_Line'Access);
   Ada.Text_IO.Put_Line ("$ " & Cmd_6);
   Command_Line.Run (Cmd_6, Put'Access, Put_Line'Access);
end TC_CMD_Filesystem;

