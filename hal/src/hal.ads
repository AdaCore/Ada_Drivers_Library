with Interfaces.Bit_Types; use Interfaces.Bit_Types;

package HAL is
   pragma Pure;

   type Byte_Array is array (Natural range <>) of Byte;
   type Short_Array is array (Natural range <>) of Short;
   type Word_Array is array (Natural range <>) of Word;
end HAL;
