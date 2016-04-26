package HAL.I2C is

   type I2C_Status is
     (Ok,
      Err_Error,
      Err_Timeout,
      Busy);

   type I2C_Data is array (Natural range <>) of Byte;

   type I2C_Memory_Address_Size is
     (Memory_Size_8b,
      Memory_Size_16b);

   subtype I2C_Address is UInt10;

   type I2C_Port is limited interface;
   type I2C_Port_Ref is not null access all I2C_Port'Class;

   procedure Master_Transmit
     (Port    : in out I2C_Port;
      Addr    : I2C_Address;
      Data    : I2C_Data;
      Status  : out I2C_Status;
      Timeout : Natural := 1000) is abstract;

   procedure Master_Receive
     (Port    : in out I2C_Port;
      Addr    : I2C_Address;
      Data    : out I2C_Data;
      Status  : out I2C_Status;
      Timeout : Natural := 1000) is abstract;

   procedure Mem_Write
     (Port          : in out I2C_Port;
      Addr          : I2C_Address;
      Mem_Addr      : Short;
      Mem_Addr_Size : I2C_Memory_Address_Size;
      Data          : I2C_Data;
      Status        : out I2C_Status;
      Timeout       : Natural := 1000) is abstract;

   procedure Mem_Read
     (Port          : in out I2C_Port;
      Addr          : I2C_Address;
      Mem_Addr      : Short;
      Mem_Addr_Size : I2C_Memory_Address_Size;
      Data          : out I2C_Data;
      Status        : out I2C_Status;
      Timeout       : Natural := 1000) is abstract;
end HAL.I2C;
