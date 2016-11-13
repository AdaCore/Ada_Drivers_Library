with Interfaces; use Interfaces;

package HAL.Block_Drivers is

   type Block_Driver is limited interface;
   type Block_Driver_Ref is access all Block_Driver'Class;

   subtype Block is Byte_Array;

   function Read
     (This         : in out Block_Driver;
      Block_Number : Unsigned_64;
      Data         : out Block)
      return Boolean is abstract;

   function Write
     (This         : in out Block_Driver;
      Block_Number : Unsigned_64;
      Data         : Block)
      return Boolean is abstract;

end HAL.Block_Drivers;
