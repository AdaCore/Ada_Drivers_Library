with Interfaces; use Interfaces;

package Media_Reader is

   type Media_Controller is limited interface;
   type Media_Controller_Access is access all Media_Controller'Class;

   type Block is array (Unsigned_16 range <>) of Unsigned_8;

   function Block_Size
     (Controller : in out Media_Controller) return Unsigned_32 is abstract;

   function Read_Block
     (Controller   : in out Media_Controller;
      Block_Number : Unsigned_32;
      Data         : out Block) return Boolean is abstract;

end Media_Reader;
