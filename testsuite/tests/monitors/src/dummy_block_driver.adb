package body Dummy_Block_Driver is

   ----------
   -- Read --
   ----------

   overriding function Read
     (This         : in out Dummy_BD;
      Block_Number : UInt64;
      Data         : out Block)
      return Boolean
   is
      pragma Unreferenced (Block_Number);
   begin
      if This.Should_Fail then
         return False;
      else
         Data := (others => 16#DE#);
         return True;
      end if;
   end Read;

   -----------
   -- Write --
   -----------

   overriding function Write
     (This         : in out Dummy_BD;
      Block_Number : UInt64;
      Data         : Block)
      return Boolean
   is
      pragma Unreferenced (Block_Number, Data);
   begin
      return not This.Should_Fail;
   end Write;

end Dummy_Block_Driver;
