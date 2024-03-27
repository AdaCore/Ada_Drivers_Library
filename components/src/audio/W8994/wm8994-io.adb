package body WM8994.IO is

   ---------------
   -- I2C_Write --
   ---------------

   procedure I2C_Write
     (This     : in out WM8994_Device;
      Register : Register_Address;
      Value    : UInt16)
   is
      Status : I2C_Status;
      Data   : I2C_Data (1 .. 2);
      Check  : UInt16 with Unreferenced;
   begin
      --  Device is MSB first
      Data (1) := UInt8 (Shift_Right (Value and 16#FF00#, 8));
      Data (2) := UInt8 (Value and 16#FF#);

      This.Port.Mem_Write
        (Addr          => This.I2C_Addr,
         Mem_Addr      => UInt16 (Register),
         Mem_Addr_Size => Memory_Size_16b,
         Data          => Data,
         Status        => Status);

      if Register /= 0 then
         Check := I2C_Read (This, Register);
      end if;
   end I2C_Write;

   --------------
   -- I2C_Read --
   --------------

   function I2C_Read
     (This     : in out WM8994_Device;
      Register : Register_Address)
   return UInt16
   is
      Status : I2C_Status;
      Data   : I2C_Data (1 .. 2);
      Result : UInt16;
   begin
      This.Port.Mem_Read
        (Addr          => This.I2C_Addr,
         Mem_Addr      => UInt16 (Register),
         Mem_Addr_Size => Memory_Size_16b,
         Data          => Data,
         Status        => Status);
      Result := Shift_Left (UInt16 (Data (1)), 8) or UInt16 (Data (2));
      return Result;
   end I2C_Read;

end WM8994.IO;
