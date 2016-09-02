package body File_Block_Drivers is

   ----------
   -- Read --
   ----------

   overriding
   function Read
     (This         : in out File_Block_Driver;
      Block_Number : Unsigned_32;
      Data         : out Block)
      return Boolean
   is
   begin

      if This.File.Seek (IO_Count (Block_Number * 512)) /= Status_Ok then
         return False;
      end if;

      return This.File.Read (Data) = Status_Ok;
   end Read;

   ----------
   -- Read --
   ----------

   overriding
   function Write
     (This         : in out File_Block_Driver;
      Block_Number : Unsigned_32;
      Data         : Block)
      return Boolean
   is
   begin

      if This.File.Seek (IO_Count (Block_Number * 512)) /= Status_Ok then
         return False;
      end if;

      return This.File.Write (Data) = Status_Ok;
   end Write;

end File_Block_Drivers;
