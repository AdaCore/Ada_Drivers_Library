with Ada.Text_IO; use Ada.Text_IO;
with HAL;         use HAL;
with Hex_Dump;

procedure TC_Hexdump is

   Data : UInt8_Array (1 .. 650);
   Cnt  : UInt8 := 0;
begin

   for Elt of Data loop
      Elt := Cnt;
      Cnt := Cnt + 1;
   end loop;

   Hex_Dump.Hex_Dump (Data     => Data,
                      Put_Line => Ada.Text_IO.Put_Line'Access,
                      Base_Addr => 16#1_0000#);
end TC_Hexdump;
