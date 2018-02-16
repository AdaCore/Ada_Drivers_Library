with Ada.Text_IO; use Ada.Text_IO;

with Command_Line;

procedure TC_CMD_Builtins is

   Cmd_1 : aliased constant String := "help";
   Cmd_2 : aliased constant String := "help echo";
   Cmd_3 : aliased constant String := "echo -n test";
   Cmd_4 : aliased constant String := "echo test";
begin
   Ada.Text_IO.Put_Line ("$ " & Cmd_1);
   Command_Line.Run (Cmd_1, Put'Access, Put_Line'Access);
   Ada.Text_IO.Put_Line ("$ " & Cmd_2);
   Command_Line.Run (Cmd_2, Put'Access, Put_Line'Access);
   Ada.Text_IO.Put_Line ("$ " & Cmd_3);
   Command_Line.Run (Cmd_3, Put'Access, Put_Line'Access);
   Ada.Text_IO.Put_Line ("$ " & Cmd_4);
   Command_Line.Run (Cmd_4, Put'Access, Put_Line'Access);
end TC_CMD_Builtins;
