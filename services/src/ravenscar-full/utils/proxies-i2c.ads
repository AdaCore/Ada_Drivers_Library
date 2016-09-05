
with System;
with HAL.I2C; use HAL.I2C;
with HAL; use HAL;

--  I2C_Proxy provides secure shared access to an I2C_Port across multiple
--  tasks.
--
--  Example:
--
--  Proxy : aliased I2C_Proxy (Target_Port      => I2C_1'Access,
--                             Ceiling_Priority => System.Priority'Last);
--
--  Device_1 : My_I2C_Device (Port => Proxy'Access);
--  --  A device used in Task 1
--
--  Device_2 : My_I2C_Device (Port => Proxy'Access);
--  --  A device used in Task 2
--

package Proxies.I2C is

   protected type I2C_Proxy (Target_Port      : not null I2C_Port_Ref;
                             Ceiling_Priority : System.Any_Priority) is
        new I2C_Port with
      pragma Priority (Ceiling_Priority);

      overriding
      entry Master_Transmit
        (Addr    : I2C_Address;
         Data    : I2C_Data;
         Status  : out I2C_Status;
         Timeout : Natural := 1000);

      overriding
      entry Master_Receive
        (Addr    : I2C_Address;
         Data    : out I2C_Data;
         Status  : out I2C_Status;
         Timeout : Natural := 1000);

      overriding
      entry Mem_Write
        (Addr          : I2C_Address;
         Mem_Addr      : Short;
         Mem_Addr_Size : I2C_Memory_Address_Size;
         Data          : I2C_Data;
         Status        : out I2C_Status;
         Timeout       : Natural := 1000);

      overriding
      entry Mem_Read
        (Addr          : I2C_Address;
         Mem_Addr      : Short;
         Mem_Addr_Size : I2C_Memory_Address_Size;
         Data          : out I2C_Data;
         Status        : out I2C_Status;
         Timeout       : Natural := 1000);
   end I2C_Proxy;
end Proxies.I2C;
