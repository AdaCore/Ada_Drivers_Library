with Ada.Text_IO;           use Ada.Text_IO;

with HAL.Block_Drivers;
with Monitor.Block_Drivers; use Monitor.Block_Drivers;
with Dummy_Block_Driver;    use Dummy_Block_Driver;

procedure TC_Block_Driver is
   BD  : aliased Dummy_BD;
   Mon : Block_Driver_Monitor (BD'Unchecked_Access,
                               Put_Line'Access);

   Data : HAL.Block_Drivers.Block (1 .. 512);

   Unref : Boolean with Unreferenced;
begin

   Data := (others => 0);

   BD.Should_Fail := True;
   Unref := Mon.Read (0, Data);
   Unref := Mon.Write (1, Data);

   BD.Should_Fail := False;
   Unref := Mon.Read (2, Data);
   Unref := Mon.Write (3, Data);
end TC_Block_Driver;
