
with Ada.Text_IO;
with Test; use Test;

procedure TC_Managed_Buffers is
begin
   SAMB.Initialize;
   Test_SAMB.Run_Test ("SAMB");

   Test_DAMB.Run_Test ("DAMB");

   SSAMB.Initialize;
   Test_SSAMB.Run_Test ("SSAMB");

   Test_SDAMB.Run_Test ("SDAMB");

   Ada.Text_IO.Put_Line ("PASS");
end TC_Managed_Buffers;
