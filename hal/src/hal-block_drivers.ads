with Interfaces; use Interfaces;

package HAL.Block_Drivers is

   type Block_Driver is limited interface;
   type Block_Driver_Ref is access all Block_Driver'Class;

   subtype Block is Byte_Array;

   Block_Size : constant := 512;
   --  Size of a block, for block number.

   function Read
     (This         : in out Block_Driver;
      Block_Number : Unsigned_64;
      Data         : out Block)
     return Boolean is abstract
     with Pre'Class => Data'Length mod Block_Size = 0;

   function Write
     (This         : in out Block_Driver;
      Block_Number : Unsigned_64;
      Data         : Block)
     return Boolean is abstract
     with Pre'Class => Data'Length mod Block_Size = 0;

end HAL.Block_Drivers;
