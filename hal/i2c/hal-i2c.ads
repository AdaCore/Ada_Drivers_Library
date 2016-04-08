with Interfaces; use Interfaces;

package HAL.I2C is
   pragma Pure;

   type I2C_Status is
     (Ok, I2C_Timeout, I2C_Error, I2C_Busy);

   type I2C_Memory_Address_Size is
     (Memory_Size_8b,
      Memory_Size_16b);

   type I2C_Data is array (Natural range <>) of Unsigned_8;

   type I2C_Controller is interface;

   type I2C_Address is mod 2 ** 10 with Size => 10;

   function Is_Configured
     (Port : I2C_Controller) return Boolean is abstract;

   procedure Master_Transmit
     (Port    : in out I2C_Controller;
      Addr    : I2C_Address;
      Data    : I2C_Data;
      Status  : out I2C_Status;
      Timeout : Natural := 1000) is abstract;

   procedure Master_Receive
     (Port    : in out I2C_Controller;
      Addr    : I2C_Address;
      Data    : out I2C_Data;
      Status  : out I2C_Status;
      Timeout : Natural := 1000) is abstract;

   procedure Mem_Write
     (Port          : in out I2C_Controller;
      Addr          : I2C_Address;
      Mem_Addr      : Unsigned_16;
      Mem_Addr_Size : I2C_Memory_Address_Size;
      Data          : I2C_Data;
      Status        : out I2C_Status;
      Timeout       : Natural := 1000) is abstract;

   procedure Mem_Read
     (Port          : in out I2C_Controller;
      Addr          : I2C_Address;
      Mem_Addr      : Unsigned_16;
      Mem_Addr_Size : I2C_Memory_Address_Size;
      Data          : out I2C_Data;
      Status        : out I2C_Status;
      Timeout       : Natural := 1000) is abstract;

end HAL.I2C;
