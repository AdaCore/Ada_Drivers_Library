package body Proxies.I2C is

   ---------------
   -- I2C_Proxy --
   ---------------

   protected body I2C_Proxy is

      ---------------------
      -- Master_Transmit --
      ---------------------

      entry Master_Transmit
        (Addr    : I2C_Address;
         Data    : I2C_Data;
         Status  : out I2C_Status;
         Timeout : Natural := 1000)
        when True
      is
      begin
         Target_Port.Master_Transmit (Addr, Data, Status, Timeout);
      end Master_Transmit;

      --------------------
      -- Master_Receive --
      --------------------

      entry Master_Receive
        (Addr    : I2C_Address;
         Data    : out I2C_Data;
         Status  : out I2C_Status;
         Timeout : Natural := 1000)
        when True
      is
      begin
         Target_Port.Master_Receive (Addr, Data, Status, Timeout);
      end Master_Receive;

      ---------------
      -- Mem_Write --
      ---------------

      entry Mem_Write
        (Addr          : I2C_Address;
         Mem_Addr      : Short;
         Mem_Addr_Size : I2C_Memory_Address_Size;
         Data          : I2C_Data;
         Status        : out I2C_Status;
         Timeout       : Natural := 1000)
        when True
      is
      begin
         Target_Port.Mem_Write (Addr, Mem_Addr, Mem_Addr_Size, Data, Status, Timeout);
      end Mem_Write;

      --------------
      -- Mem_Read --
      --------------

      entry Mem_Read
        (Addr          : I2C_Address;
         Mem_Addr      : Short;
         Mem_Addr_Size : I2C_Memory_Address_Size;
         Data          : out I2C_Data;
         Status        : out I2C_Status;
         Timeout       : Natural := 1000)
        when True
      is
      begin
         Target_Port.Mem_Read (Addr, Mem_Addr, Mem_Addr_Size, Data, Status, Timeout);
      end Mem_Read;

   end I2C_Proxy;

end Proxies.I2C;
