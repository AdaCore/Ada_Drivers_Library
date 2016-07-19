with HAL.I2C;
with HAL; use HAL;
with MCP23x08; use MCP23x08;

package MCP23008 is

   type MCP23008_Device (Port : HAL.I2C.I2C_Port_Ref;
                         Addr : UInt3) is
     new MCP23x08_Device with private;

   overriding
   procedure IO_Write
     (This      : in out MCP23008_Device;
      WriteAddr : Register_Address;
      Value     : Byte);

   overriding
   procedure IO_Read
     (This     : MCP23008_Device;
      ReadAddr : Register_Address;
      Value    : out Byte);

private
   type MCP23008_Device (Port : HAL.I2C.I2C_Port_Ref;
                         Addr : UInt3) is
     new MCP23x08_Device with null record;

   BASE_ADDRESS : constant HAL.I2C.I2C_Address := 16#40#;
end MCP23008;
