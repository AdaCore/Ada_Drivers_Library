with HAL.I2C; use HAL.I2C;
with HAL; use HAL;

package I2C_Port_Printer is

   type Put_Line_Procedure is access procedure (Str : String);

   type I2C_Printer (Real_Port : I2C_Port_Ref;
                     Put_Line  : not null Put_Line_Procedure) is
     new I2C_Port with private;

   type I2C_Printer_Ref is access all I2C_Printer'Class;

   overriding
   procedure Master_Transmit
     (This    : in out I2C_Printer;
      Addr    : I2C_Address;
      Data    : I2C_Data;
      Status  : out I2C_Status;
      Timeout : Natural := 1000);

   overriding
   procedure Master_Receive
     (This    : in out I2C_Printer;
      Addr    : I2C_Address;
      Data    : out I2C_Data;
      Status  : out I2C_Status;
      Timeout : Natural := 1000);

   overriding
   procedure Mem_Write
     (This          : in out I2C_Printer;
      Addr          : I2C_Address;
      Mem_Addr      : Short;
      Mem_Addr_Size : I2C_Memory_Address_Size;
      Data          : I2C_Data;
      Status        : out I2C_Status;
      Timeout       : Natural := 1000);

   overriding
   procedure Mem_Read
     (This          : in out I2C_Printer;
      Addr          : I2C_Address;
      Mem_Addr      : Short;
      Mem_Addr_Size : I2C_Memory_Address_Size;
      Data          : out I2C_Data;
      Status        : out I2C_Status;
      Timeout       : Natural := 1000);

private
   type I2C_Printer (Real_Port : I2C_Port_Ref;
                     Put_Line  : not null Put_Line_Procedure) is
     new I2C_Port with null record;
end I2C_Port_Printer;
