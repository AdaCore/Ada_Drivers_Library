
package body FT801.Registers is

   --------------------
   -- Write_Register --
   --------------------

   procedure Write_Register (This : FT801_Device;
                             Reg  : Register;
                             Val  : UInt32)
   is
      Arr : UInt8_Array (1 .. 4)
        with Address => Val'Address;
   begin
      Host_Memory_Write (This    => This,
                         Address => Register'Enum_Rep (Reg),
                         Payload => Arr);
   end Write_Register;

   -------------------
   -- Read_Register --
   -------------------

   procedure Read_Register (This : FT801_Device;
                            Reg  : Register;
                            Val  : out UInt32)
   is
      Arr : UInt8_Array (1 .. 4)
        with Address => Val'Address;
   begin
      Host_Memory_Read (This    => This,
                        Address => Register'Enum_Rep (Reg),
                        Payload => Arr);
   end Read_Register;

end FT801.Registers;
