--  Simulate a disk by readind into a file

with HAL.Block_Drivers; use HAL.Block_Drivers;
with HAL.Filesystem;    use HAL.Filesystem;
with Interfaces;        use Interfaces;

package File_Block_Drivers is

   type File_Block_Driver (File : not null File_Handle_Ref) is new Block_Driver with private;

   overriding
   function Read
     (This         : in out File_Block_Driver;
      Block_Number : Unsigned_32;
      Data         : out Block)
      return Boolean;

   overriding
   function Write
     (This         : in out File_Block_Driver;
      Block_Number : Unsigned_32;
      Data         : Block)
      return Boolean;

private
   type File_Block_Driver (File : not null File_Handle_Ref) is new Block_Driver with null record;
end File_Block_Drivers;
