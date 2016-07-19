with HAL.I2C; use HAL.I2C;

package body MCP23008 is

   --------------
   -- IO_Write --
   --------------

   overriding
   procedure IO_Write
     (This      : in out MCP23008_Device;
      WriteAddr : Register_Address;
      Value     : Byte)
   is
      Status : I2C_Status;
   begin
      This.Port.Mem_Write
        (BASE_ADDRESS or I2C_Address (This.Addr),
         Short (WriteAddr),
         Memory_Size_8b,
         (1 => Value),
         Status,
         1000);

      if Status /= Ok then
         --  No error handling...
         raise Program_Error;
      end if;
   end IO_Write;

   -------------
   -- IO_Read --
   -------------

   overriding
   procedure IO_Read
     (This     : MCP23008_Device;
      ReadAddr : Register_Address;
      Value    : out Byte)
   is
      Ret    : I2C_Data (1 .. 1);
      Status : I2C_Status;
   begin
      This.Port.Mem_Read
        (BASE_ADDRESS or I2C_Address (This.Addr),
         Short (ReadAddr),
         Memory_Size_8b,
         Ret,
         Status,
         1000);

      if Status /= Ok then
         --  No error handling...
         raise Program_Error;
      end if;

      Value := Ret (1);
   end IO_Read;

end MCP23008;
