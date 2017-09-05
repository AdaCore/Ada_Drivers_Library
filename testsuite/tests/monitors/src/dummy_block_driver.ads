with HAL;               use HAL;
with HAL.Block_Drivers; use HAL.Block_Drivers;

package Dummy_Block_Driver is

   type Dummy_BD is new Block_Driver with record
      Should_Fail : Boolean;
   end record;

   overriding
   function Read
     (This         : in out Dummy_BD;
      Block_Number : UInt64;
      Data         : out Block)
      return Boolean;

   overriding
   function Write
     (This         : in out Dummy_BD;
      Block_Number : UInt64;
      Data         : Block)
      return Boolean;

end Dummy_Block_Driver;
