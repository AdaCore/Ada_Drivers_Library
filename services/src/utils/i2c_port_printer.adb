package body I2C_Port_Printer is

   ---------------------
   -- Master_Transmit --
   ---------------------

   overriding procedure Master_Transmit
     (This    : in out I2C_Printer;
      Addr    : I2C_Address;
      Data    : I2C_Data;
      Status  : out I2C_Status;
      Timeout : Natural := 1000)
   is
   begin
      This.Put_Line ("I2C_Port: Call Master_Transmit (Addr:" & Addr'Img &
                       ", Data'Length:" & Data'Length'Img &
                       ", Status:[out], Timeout:" & Timeout'Img &
                       ")...");
      if This.Real_Port /= null then
         This.Real_Port.Master_Transmit (Addr, Data, Status, Timeout);
      else
         Status := Ok;
      end if;
      This.Put_Line ("I2C_Port: Return from Master_Transmit Status:" &
                       Status'Img & ".");
   end Master_Transmit;

   --------------------
   -- Master_Receive --
   --------------------

   overriding procedure Master_Receive
     (This    : in out I2C_Printer;
      Addr    : I2C_Address;
      Data    : out I2C_Data;
      Status  : out I2C_Status;
      Timeout : Natural := 1000)
   is
   begin
      This.Put_Line ("I2C_Port: Call Master_Receive (Addr:" & Addr'Img &
                       ", Data'Length:" & Data'Length'Img &
                       ", Status:[out], Timeout:" & Timeout'Img &
                       ")...");
      if This.Real_Port /= null then
         This.Real_Port.Master_Receive (Addr, Data, Status, Timeout);
      else
         for Elt of Data loop
            Elt := 0;
         end loop;
         Status := Ok;
      end if;
      This.Put_Line ("I2C_Port: Return from Master_Receive Status:" &
                       Status'Img & ".");
   end Master_Receive;

   ---------------
   -- Mem_Write --
   ---------------

   overriding procedure Mem_Write
     (This          : in out I2C_Printer;
      Addr          : I2C_Address;
      Mem_Addr      : Short;
      Mem_Addr_Size : I2C_Memory_Address_Size;
      Data          : I2C_Data;
      Status        : out I2C_Status;
      Timeout       : Natural := 1000)
   is
   begin
      This.Put_Line ("I2C_Port: Call Mem_Write (Addr:" & Addr'Img &
                       ", Mem_Addr:" & Mem_Addr'Img &
                       ", Mem_Addr_Size:" & Mem_Addr_Size'Img &
                       ", Data'Length:" & Data'Length'Img &
                       ", Status:[out], Timeout:" & Timeout'Img &
                       ")...");
      if This.Real_Port /= null then
         This.Real_Port.Mem_Write (Addr, Mem_Addr, Mem_Addr_Size, Data, Status, Timeout);
      else
         Status := Ok;
      end if;
      This.Put_Line ("I2C_Port: Return from Mem_Write Status:" &
                       Status'Img & ".");
   end Mem_Write;

   --------------
   -- Mem_Read --
   --------------

   overriding procedure Mem_Read
     (This          : in out I2C_Printer;
      Addr          : I2C_Address;
      Mem_Addr      : Short;
      Mem_Addr_Size : I2C_Memory_Address_Size;
      Data          : out I2C_Data;
      Status        : out I2C_Status;
      Timeout       : Natural := 1000)
   is
   begin
      This.Put_Line ("I2C_Port: Call Mem_Read (Addr:" & Addr'Img &
                       ", Mem_Addr:" & Mem_Addr'Img &
                       ", Mem_Addr_Size:" & Mem_Addr_Size'Img &
                       ", Data'Length:" & Data'Length'Img &
                       ", Status:[out], Timeout:" & Timeout'Img &
                       ")...");
      if This.Real_Port /= null then
         This.Real_Port.Mem_Read (Addr, Mem_Addr, Mem_Addr_Size, Data, Status, Timeout);
      else
         for Elt of Data loop
            Elt := 0;
         end loop;
         Status := Ok;
      end if;
      This.Put_Line ("I2C_Port: Return from Mem_Read Status:" &
                       Status'Img & ".");
   end Mem_Read;

end I2C_Port_Printer;
